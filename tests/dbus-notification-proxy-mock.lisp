(in-package :cl-notify/tests)

(defclass dbus-notification-proxy-mock (standard-dbus-notification-proxy)
  ((last-method-sent :accessor last-method-sent :type list)
   (actions-support :initarg :actions-support :reader actions-support :initform nil :type boolean)))

(defmethod notify ((proxy dbus-notification-proxy-mock) summary &key app-name replaces-id app-icon body actions hints expire-timeout)
  (setf (last-method-sent proxy) (list "org.freedesktop.Notifications.Notify" app-name replaces-id app-icon summary body actions hints expire-timeout)))

(defmethod close-notification ((proxy dbus-notification-proxy-mock) notification-id)
  (setf (last-method-sent proxy) (list "org.freedesktop.Notifications.CloseNotification" notification-id)))

(defmethod get-server-information ((proxy dbus-notification-proxy-mock))
  (make-instance 'server-information :version "1.0.0" :spec-version "42" :name "Mock" :vendor "Common lisp"))

(defmethod get-server-capabilities ((proxy dbus-notification-proxy-mock))
  (let ((capabilities (remove-if #'null (list "body"
                                              (when (actions-support proxy) "actions")))))
    (make-array (length capabilities) :element-type 'string :initial-contents capabilities)))

(defun dbus-method-sent (mock-dbus-proxy &rest args)
  (loop :for parameter :in (last-method-sent mock-dbus-proxy)
        :for argument :in args
        :always (let ((was-sent (equal parameter argument)))
                  (unless was-sent (format t "~%The argument ~a was not sent to the server. Intead ~a got sent.~%" argument parameter))
                  was-sent)))

(defun mock-dbus-proxy-with-actions-support ()
  (make-instance 'dbus-notification-proxy-mock :actions-support t))
