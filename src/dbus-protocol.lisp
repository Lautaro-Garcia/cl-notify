(in-package :cl-notify)

(defclass standard-dbus-notification-proxy () ())

(defgeneric notify (dbus-notification-proxy summary &key app-name replaces-id app-icon body actions hints expire-timeout))

(defgeneric get-server-information (dbus-notification-proxy))

(defgeneric get-server-capabilities (dbus-notification-proxy))


(defclass server-information ()
  ((name :initarg :name :reader name :type string);
   (vendor :initarg :vendor :reader vendor :type string)
   (version :initarg :version :reader version :type string)
   (spec-version :initarg :spec-version :reader spec-version :type string)))

(defmethod print-object ((server-info server-information) stream)
  (print-unreadable-object (server-info stream :type t)
    (format stream "name=~s version=~s vendor=~s spec-version=~s"
            (name server-info) (version server-info) (version server-info) (spec-version server-info)))
  server-info)

(defclass dbus-notification-proxy (standard-dbus-notification-proxy)
  ((dbus-notifications-object :accessor dbus-notifications-object :type function)))

(defmacro with-dbus-method-inovaction ((result-var method &rest args) &body body)
  (alexandria:with-gensyms (bus notifications-object)
    `(dbus:with-open-bus (,bus (dbus:session-server-addresses))
       (dbus:with-introspected-object (,notifications-object ,bus "/org/freedesktop/Notifications" "org.freedesktop.Notifications")
         (let ((,result-var (,notifications-object "org.freedesktop.Notifications" ,method ,@args)))
          ,@body)))))

(defmethod notify ((proxy dbus-notification-proxy) summary &key app-name replaces-id app-icon body actions hints expire-timeout)
  (with-dbus-method-inovaction (notification-id "Notify" app-name replaces-id app-icon summary body actions hints expire-timeout)
    notification-id))

(defmethod get-server-information ((proxy dbus-notification-proxy))
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object (notifications-object bus "/org/freedesktop/Notifications" "org.freedesktop.Notifications")
      (multiple-value-bind (name vendor version spec-version) (notifications-object "org.freedesktop.Notifications" "GetServerInformation")
        (make-instance 'server-information :name name :vendor vendor :version version :spec-version spec-version)))))

(defmethod get-server-capabilities ((proxy dbus-notification-proxy))
  (with-dbus-method-inovaction (capabilities "GetCapabilities")
    capabilities))

(defmethod close-notification((proxy dbus-notification-proxy) notification-id)
  (with-dbus-method-inovaction (response "CloseNotification" notification-id)
    (declare (ignore response))))
