(in-package :cl-notify)

(defclass notification-expired () ())

(defclass notification-dismissed () ())

(defclass notification-closed-programatically () ())

(defclass notification-closed-for-undefined-reasons () ())

(defmethod make-notification-closed-reason (reason-id)
  (make-instance 'notification-closed-for-undefined-reasons))

(defmethod make-notification-closed-reason ((reason-id (eql 1)))
  (make-instance 'notification-expired))

(defmethod make-notification-closed-reason ((reason-id (eql 2)))
  (make-instance 'notification-dismissed))

(defmethod make-notification-closed-reason ((reason-id (eql 3)))
  (make-instance 'notification-closed-programatically))
