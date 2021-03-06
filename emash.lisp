(defpackage emash
  (:use cl)
  (:import-from whirlog do-context let-tables with-db)
  (:export start))

(in-package emash)

(defparameter *commands*
  `(("add-account" . ,(lambda ()
			(format t "Adding account...~%")))))

(defun find-command (x)
  (rest (assoc x *commands* :test #'string=)))

(defun start ()
  (let-tables ((smtp-accounts (e-mail :primary-key? t) address user password))
    (with-db ("db/" smtp-accounts)
      (tagbody
       next
	 (format t "emash> ")
	 (let ((in (read-line *standard-input* nil)))
           (when (and in (not (string= in "")))
	     (let ((c (find-command in)))
	       (if c
		   (do-context () (funcall c))
		   (format t "Unknown command: ~a~%" in)))
	     (go next))))
      (format t "Quitting...~%"))))
