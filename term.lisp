(defpackage term
  (:use cl)
  (:export fcolor reset))

(in-package term)

(defun fcolor (red green blue &optional out)
  (format out "~c[38;2;~a;~a;~am" #\esc red green blue))

(defun reset (&optional out)
  (format out "~cm" #\esc))
