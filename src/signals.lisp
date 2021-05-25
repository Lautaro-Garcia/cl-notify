(in-package :cl-notify)

(deftype callback-type ()
  '(member :close :action))

(defvar *signal-handler* nil)

(stmx:transactional
  (defclass signal-handler ()
    ((close-callbacks :initform (make-hash-table) :reader close-callbacks :type hash-table)
     (action-callbacks :initform (make-hash-table) :reader action-callbacks :type hash-table)
     (signal-handling-loop-thread :accessor signal-handling-loop-thread :type bt:thread))))

(defun register-callback-to (handler type callback-name callback)
  (stmx:atomic
   (ecase type
     (:close (setf (gethash callback-name (close-callbacks handler)) callback))
     (:action (setf (gethash callback-name (action-callbacks handler)) callback)))))

(defun unregister-callback-to (handler type callback-name)
  (stmx:atomic
   (ecase type
     (:close (remhash callback-name (close-callbacks handler)))
     (:action (remhash callback-name (action-callbacks handler))))))

(defun register-callback (type callback-name callback)
  (check-type type callback-type)
  (ensure-signal-handler)
  (register-callback-to *signal-handler* type callback-name callback))

(defun unregister-callback (type callback-name)
  (ensure-signal-handler)
  (unregister-callback-to *signal-handler* type callback-name))

(defun execute-callbacks (handler type args)
  (let ((callbacks '()))
    (stmx:atomic
      (let ((callback-table (ecase type (:close (close-callbacks handler)) (:action (action-callbacks handler)))))
        (loop :for callback :being :the :hash-value :of callback-table :doing (push callback callbacks))))
    (loop :for callback :in callbacks :do (apply callback args))))

(defun run-signal-handling-loop ()
  (handler-case
      (dbus:with-open-bus (bus (dbus:session-server-addresses))
        (dbus:add-match bus :type "signal" :sender "org.freedesktop.Notifications")
        (dbus:publish-objects bus))
    (end-of-file ()
      :disconnected-by-bus)))

(defun stop-signal-handling-loop ()
  (bt:destroy-thread (signal-handling-loop-thread *signal-handler*))
  (setf *signal-handler* nil))

(defun start-signal-handling-loop (handler)
  (dbus:define-dbus-object notifications-object
    (:path "/org/freedesktop/Notifications"))

  (dbus:define-dbus-signal-handler (notifications-object notification-closed) ((notification-id :uint32) (close-reason :uint32))
    (:name "NotificationClosed")
    (:interface "org.freedesktop.Notifications")
    (execute-callbacks handler :close (list notification-id (make-notification-closed-reason close-reason))))

  (dbus:define-dbus-signal-handler (notifications-object action-invoked) ((notification-id :uint32) (action-key :string))
    (:name "ActionInvoked")
    (:interface "org.freedesktop.Notifications")
    (execute-callbacks handler :action (list notification-id action-key)))

  (let ((new-thread (bt:make-thread #'run-signal-handling-loop :name "cl-notify event loop")))
    (stmx:atomic (setf (signal-handling-loop-thread handler) new-thread))))

(defun ensure-signal-handler ()
  (unless *signal-handler*
    (setf *signal-handler* (make-instance 'signal-handler))
    (start-signal-handling-loop *signal-handler*)))

(defmacro define-callback (name type (notification-id-var action-key-or-reason-var) &body body)
  `(register-callback ,type ',name (lambda (,notification-id-var ,action-key-or-reason-var) (progn ,@body))))
