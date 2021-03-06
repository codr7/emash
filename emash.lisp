(defpackage emash
  (:use cl)
  (:import-from whirlog column-value do-context find-record let-tables new-record store-record with-db)
  (:export start))

(in-package emash)

(defvar *settings*)
(defvar *smtp-accounts*)

(defparameter *default-smtp-account* :default-smtp-account)

(defun find-setting (key)
  (column-value (find-record *settings* `(,key)) 'value))

(defun (setf find-setting) (val key)
  (store-record *settings* (new-record 'key key 'value val)))

(defun ask (prompt)
  (format t prompt)
  (read-line *standard-input* nil))

(defun ask-y/n (prompt &key (default nil))
  (let ((in (ask (format nil "~a (~a/~a)? " prompt (if default "[y]" "y") (if default "n" "[n]")))))
    (cond
      ((or (string= in "y") (and default (string= in "")))
       t)
      ((or (string= in "n") (and (not default) (string= in "")))
       nil)
      (t
       (ask-y/n prompt :default default)))))

(defparameter *commands*
  `(("add-smtp-account" . ,(lambda ()
			     (let ((e-mail (ask "e-mail: ")))
			       (ask "host: ")
			       (ask "port: ")
			       (ask "user: ")
			       (ask "password: ")
			       (format t "created SMTP account: '~a'~%" e-mail)
			       (when (ask-y/n "make default")
				 (setf (find-setting *default-smtp-account*) e-mail)
				 (format t "'~a' is default SMTP account~%" e-mail)))))))


(defun find-command (x)
  (rest (assoc x *commands* :test #'string=)))

(defun start ()
  (let-tables ((*settings* (key :primary-key? t) value)
	       (*smtp-accounts* (e-mail :primary-key? t) host port user password))
    (with-db (nil *settings* *smtp-accounts*)
      (tagbody
       next
	 (let ((in (ask "emash> ")))
           (when (and in (not (string= in "")))
	     (let ((c (find-command in)))
	       (if c
		   (do-context () (funcall c))
		   (format t "unknown command: ~a~%" in)))
	     (go next))))
      (format t "over, out!~%"))))
