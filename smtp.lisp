(defpackage smtp
  (:import-from curl curl-easy-perform curl-slist-append curl-slist-free-all)
  (:use cl cffi)
  (:export send-message))

(in-package smtp)

(defun crlf (out)
  (write-char #\return out)
  (write-char #\linefeed out))

(defvar *read-callback*)

(defctype size :unsigned-int)

(defcallback read-message size ((ptr :pointer) (size size) (nmemb size) (data :pointer))
  (declare (ignore data))
  (let ((max-size (* size nmemb)))
    (funcall *read-callback* ptr max-size)))

(defun memcpy (dst src nbytes)
  (cffi:foreign-funcall "memcpy"
                        :pointer dst
                        :pointer src
                        :int nbytes
                        :void))

(defun send-message (h &key
			 (host (error "Missing :host"))
			 (port (error "Missing :port"))
			 (user (error "Missing :user"))
			 (password (error "Missing :password"))
			 (from (error "Missing :from"))
			 (to (error "Missing :to"))
			 (subject (error "Missing :subject"))
			 (body (error "Missing :body")))
  (curl:ok? (curl:setopt h :url :string (format nil "smtps://~a:~a" host port)))
  (curl:ok? (curl:setopt h :username :string user))
  (curl:ok? (curl:setopt h :password :string password))
  (curl:ok? (curl:setopt h :mail-from :string from))
  (curl:ok? (curl:setopt h :readfunction :pointer (callback read-message)))
  (curl:ok? (curl:setopt h :upload :long 1))

  (let ((rcpt (curl-slist-append (null-pointer) to)))
    (curl:ok? (curl:setopt h :mail-rcpt :pointer rcpt))
    
    (with-foreign-string ((mptr msize) (with-output-to-string (msg)
					 (write-string "Subject: " msg)
					 (write-string subject msg)
					 (crlf msg)
					 (write-string body msg)))
      (let* ((mptr mptr)
	     (*read-callback* (lambda (ptr max-size)
				(let ((s (min msize max-size)))
				  (unless (zerop s)
				    (memcpy ptr mptr s)
				    (incf-pointer mptr s)
				    (decf msize s))
				 s))))
	(curl:ok? (curl-easy-perform h))))
    (curl-slist-free-all rcpt)))
