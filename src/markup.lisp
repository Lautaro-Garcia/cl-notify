(in-package :cl-notify)

(defclass standard-notification-markup () ())

(defclass no-markup (standard-notification-markup) ())

(defclass body-notification-markup (standard-notification-markup) ())

(defgeneric tag->string (markup tag form)
  (:documentation "Converts to string the FORM tagged with TAG form the markup type specified by MARKUP."))

(defmethod tag->string (markup tag form)
  (handler-case (or (second form) "") (error () "")))

(defmethod tag->string ((markup body-notification-markup) (tag (eql :b)) form)
  (format nil "<b>~a</b>" (second form)))

(defmethod tag->string ((markup body-notification-markup) (tag (eql :i)) form)
  (format nil "<i>~a</i>" (second form)))

(defmethod tag->string ((markup body-notification-markup) (tag (eql :u)) form)
  (format nil "<u>~a</u>" (second form)))

(defmethod tag->string ((markup body-notification-markup) (tag (eql :a)) form)
  (format nil "<a href=\"~a\">~a</a>" (third form) (second form)))

(defmethod tag->string ((markup no-markup) (tag (eql :img)) form)
  "")

(defmethod tag->string ((markup body-notification-markup) (tag (eql :img)) form)
  (format nil "<img src=\"~a\" alt=\"~a\"/>" (second form) (third form)))

(defmethod tag->string ((markup body-notification-markup) (tag (eql :br)) form)
  (declare (ignore form))
  "<br>")

(defgeneric as-string (markup form)
  (:documentation "Converts the FORM to string according to the MARKUP type."))

(defmethod as-string (markup (form string))
  form)

(defmethod as-string (markup (form list))
  (tag->string markup (car form) form))

(defmethod markup->string (markup (text string))
  text)

(defmethod markup->string (markup (text list))
  (if (keywordp (first text))
    (as-string markup text)
    (apply #'concatenate (cons 'string (loop :for form :in text :collecting (as-string markup form))))))
