(defpackage curl
  (:use cl cffi)
  (:export do-handle new-handle ok? setopt
	   curl-easy-perform curl-slist-append curl-slist-free-all))

(in-package curl)

(define-foreign-library curl
  (t (:default "libcurl")))

(use-foreign-library curl)

(defctype curl-handle :pointer)

(defcfun "curl_easy_init" curl-handle)
(defcfun "curl_easy_cleanup" :void (h curl-handle))

(defctype curl-code :int)

(defcfun "curl_easy_strerror" :string (res curl-code))

(defun ok? (res)
  (unless (zerop res)
    (error (curl-easy-strerror res))))

(defcenum curl-option
  (:verbose 41)
  (:upload 46)
  (:url 10002)
  (:username 10173)
  (:password 10174)
  (:mail-from 10186)
  (:mail-rcpt 10187)
  (:readfunction 20012))

(defmacro setopt (h opt typ val)
  `(foreign-funcall "curl_easy_setopt"
		    :pointer ,h
		    curl-option ,opt
		    ,typ ,val
		    curl-code))

(defun new-handle ()
  (let ((h (curl-easy-init)))
    (ok? (setopt h :verbose :long 1))
    h))

(defmacro do-handle ((var) &body body)
  `(let ((,var (new-handle)))
     (when (null-pointer-p ,var)
       (error "Failed initializing libcurl"))
     (unwind-protect
	  (progn ,@body)
       (curl-easy-cleanup ,var))))

(defcfun "curl_easy_perform" curl-code (h curl-handle))

(defctype curl-slist :pointer)

(defcfun "curl_slist_append" curl-slist (list curl-slist) (string :string))

(defcfun "curl_slist_free_all" :void (list curl-slist))
