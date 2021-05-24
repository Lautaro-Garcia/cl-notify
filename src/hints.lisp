(in-package :cl-notify)

(defun make-hint (name types values)
  (let ((values-as-list (if (listp values) values (list values))))
    `(,name (,types ,@values-as-list))))

(defun make-urgency-hint (urgency-level)
  (make-hint "urgency" '(:byte) urgency-level))

(defun low-urgency-hint ()
  (make-urgency-hint 0))

(defun normal-urgency-hint ()
  (make-urgency-hint 1))

(defun critical-urgency-hint ()
  (make-urgency-hint 2))
