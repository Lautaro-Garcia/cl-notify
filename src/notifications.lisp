(in-package :cl-notify)

(defvar *notifications* nil)

(defclass notifications ()
  ((dbus-proxy :initarg :dbus-proxy :reader dbus-proxy :type standard-dbus-notification-proxy)
   (server-information :accessor server-information :type server-information)
   (server-capabilities :accessor server-capabilities :type list)
   (body-markup :accessor body-markup :type standard-notification-markup)))

(defclass notification ()
  ((id :reader id :initarg :id :type integer)
   (notification-server :initarg :creator :reader creator :type notifications)
   (summary :initarg :summary :accessor summary :type string)
   (app-name :initarg :app-name :accessor app-name :type string)
   (app-icon :initarg :app-icon :accessor app-icon :type string)
   (body :initarg :body :accessor body :type string)
   (actions :initarg :actions :accessor actions :type list)
   (hints :initarg :hints :accessor hints :type list)
   (expire-timeout :initarg :expire-timeout :accessor expire-timeout :type integer)))


(defmethod initialize-instance :after ((notifications notifications) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((dbus-proxy (dbus-proxy notifications)))
    (setf (server-information notifications) (get-server-information dbus-proxy))
    (setf (server-capabilities notifications) (get-server-capabilities dbus-proxy))
    (setf (body-markup notifications) (make-instance (if (body-markup-support notifications) 'body-notification-markup 'no-markup)))))

(defmethod close-notification ((notifications notifications) notification-id)
  (close-notification (dbus-proxy notifications) notification-id))

(defmethod clear ((notification notification))
  (close-notification (creator notification) (id notification)))

(defmethod update ((notification notification) &key (summary (summary notification)) (app-name (app-name notification)) (app-icon (app-icon notification)) (body (body notification)) (actions (actions notification)) (hints (hints notification)) (expire-timeout (expire-timeout notification)))
  (update (creator notification)
          :summary summary
          :app-name app-name
          :replaces-id (id notification)
          :app-icon app-icon
          :body body
          :hints hints
          :actions actions
          :expire-timeout expire-timeout)
  (setf (summary notification) summary
        (app-name notification) app-name
        (app-icon notification) app-icon
        (body notification) body
        (hints notification) hints
        (actions notification) actions
        (expire-timeout notification) expire-timeout)
  notification)

(defmethod update ((notifications notifications) &key summary app-name app-icon body actions hints expire-timeout replaces-id)
  (notify notifications summary :app-name app-name
                                :replaces-id replaces-id
                                :app-icon app-icon
                                :body body
                                :hints hints
                                :actions actions
                                :expire-timeout expire-timeout))

(defun has-capability (notifications capability)
  (find capability (server-capabilities notifications) :test #'equal))

(defun actions-support (notifications)
  (has-capability notifications "actions"))

(defun body-markup-support (notifications)
  (has-capability notifications "body-markup"))

(defmethod notify ((notifications notifications) summary &key (app-name "") (replaces-id 0) (app-icon "") (body "") actions hints (expire-timeout -1))
  (let ((computed-actions (and (actions-support notifications) actions))
        (computed-body (markup->string (body-markup notifications) body)))
    (let ((notification-id (notify (dbus-proxy notifications) summary :app-name app-name
                                                                      :replaces-id replaces-id
                                                                      :app-icon app-icon
                                                                      :body computed-body
                                                                      :hints hints
                                                                      :actions computed-actions
                                                                      :expire-timeout expire-timeout)))
      (make-instance 'notification :id notification-id
                                   :creator notifications
                                   :summary summary
                                   :app-name app-name
                                   :app-icon app-icon
                                   :body body
                                   :actions actions
                                   :hints hints
                                   :expire-timeout expire-timeout))))

(defun send-notification (summary &key (app-name "") (replaces-id 0) (app-icon "") (body "") actions hints (timeout-in-millis -1))
  (unless *notifications* (setf *notifications* (make-instance 'notifications :dbus-proxy (make-instance 'dbus-notification-proxy))))
  (notify *notifications* summary :app-name app-name :replaces-id replaces-id :app-icon app-icon :body body :actions actions :hints hints :expire-timeout timeout-in-millis))


(defun notification-server-info ()
  (server-information *notifications*))

(defun notification-server-capabilities ()
  (server-capabilities *notifications*))
