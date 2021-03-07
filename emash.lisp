(defpackage emash
  (:use cl)
  (:import-from util kw)
  (:import-from whirlog column-value do-context do-records find-record let-tables new-record records store-record with-db)
  (:export start))

(in-package emash)

(defvar *settings*)
(defvar *smtps*)

(defparameter *default-smtp* :default-smtp)

(defun find-setting (key)
  (column-value (find-record *settings* `(,key)) 'value))

(defun (setf find-setting) (val key)
  (store-record *settings* (new-record 'key key 'value val)))

(defun ask-line (prompt)
  (format t prompt)
  (read-line *standard-input* nil))

(defun ask-string (prompt &key default)
  (let ((res (ask-line (format nil "~a~a: " prompt (if default (format nil " [~a]" default) "")))))
    (if (string= res "") default res)))

(defun ask-list (prompt lst &key default)
  (let ((res (ask-string (format nil "~a ~a-~a" prompt (if lst 1 0) (if lst (length lst) 0)) :default default)))
    (if (or (string= res "") (string= res default)) default (nth (1- (parse-integer res)) lst))))

(defun ask-y/n (prompt &key default)
  (let ((in (ask-line (format nil "~a ~a/~a? " prompt (if default "[y]" "y") (if default "n" "[n]")))))
    (cond
      ((or (string= in "y") (and default (string= in "")))
       t)
      ((or (string= in "n") (and (not default) (string= in "")))
       nil)
      (t
       (ask-y/n prompt :default default)))))

(defun say (spec &rest args)
  (apply #'format t spec args)
  (terpri)
  (force-output))

(defparameter *commands*
  `((:add-smtp . ,(lambda ()
		   (let ((e-mail (ask-string "e-mail")))
		     (let ((prev (find-record *smtps* `(,e-mail))))
		       (let ((host (ask-string "host" :default (when prev (column-value prev 'host))))
			     (port (parse-integer (ask-string "port" :default (when prev (column-value prev 'port)))))
			     (user (ask-string "user" :default (when prev (column-value prev 'user))))
			     (password (ask-string "password" :default (when prev (column-value prev 'password)))))
			 (store-record *smtps* (new-record 'e-mail e-mail
							   'host host
							   'port port
							   'user user
							   'password password))
			 
			 (say "~a smtp ~a" (if prev "updated" "created") e-mail)
			 (unless (string= (find-setting *default-smtp*) e-mail)
			   (when (ask-y/n "make default" :default t)
			     (setf (find-setting *default-smtp*) e-mail)))
			 (when (string= (find-setting *default-smtp*) e-mail)
			   (say "~a is default smtp" e-mail)))))))
    (:send . ,(lambda ()
	       (let (smtps (i 0))
		 (do-records (rec *smtps*)
		   (let ((e-mail (column-value rec 'e-mail)))
		     (say "~a) ~a" (incf i) e-mail)
		     (push e-mail smtps)))
		 (let ((from (ask-list "from" (nreverse smtps) :default (find-setting *default-smtp*)))
		       (to (ask-string "receiver"))
		       (subj (ask-string "subject"))
		       (body (ask-line "")))
		   (say "sending now")
		   (let ((smtp (find-record *smtps* `(,from))))  
		     (curl:do-handle (h)
		       (smtp:send-message h
					  :host (column-value smtp 'host)
					  :port (column-value smtp 'port)
					  :user (column-value smtp 'user)
					  :password (column-value smtp 'password)
					  :from from
					  :to to
					  :subject subj
					  :body body)
		       (say "ok")))))))))

(defun find-command (x)
  (rest (assoc x *commands*)))

(defun start ()
  (let-tables ((*settings* (key :primary-key? t) value)
	       (*smtps* (e-mail :primary-key? t) host port user password))
    (with-db (nil *settings* *smtps*)
      (tagbody
       next
	 (let ((in (ask "emash> ")))
           (when (and in (not (string= in "")))
	     (with-input-from-string (in in)
	       (let* ((cid (read in))
		      (c (find-command (kw cid))))
		 (if c
		     (do-context () (funcall c))
		     (say "unknown: ~a" cid)))
	       (go next)))))
      (format t "over, out~%"))))
