(in-package :cl-notify/tests)

(def-suite notifications)

(in-suite notifications)

(def-fixture with-mock-dbus-proxy (&optional (dbus-proxy (make-instance 'dbus-notification-proxy-mock)))
  (let ((notifications (make-instance 'notifications :dbus-proxy dbus-proxy)))
    (&body)))

(def-test on-init-queries-the-server-for-information (:fixture with-mock-dbus-proxy)
  (is (typep (server-information notifications) 'cl-notify::server-information))
  (is (typep (server-capabilities notifications) 'simple-vector)))

(def-test show-summary-in-notification (:fixture with-mock-dbus-proxy)
  (notify notifications "Summary")
  (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.Notify" "" 0 "" "Summary" "" nil nil -1)))

(def-test show-complete-notification (:fixture with-mock-dbus-proxy)
  (notify notifications "Summary" :body "body" :app-name "app-name" :replaces-id 28 :app-icon "notification" :expire-timeout 60)
  (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.Notify" "app-name" 28 "notification" "Summary" "body" nil nil 60)))

(def-test hints-get-sent-when-server-supports-them (:fixture with-mock-dbus-proxy)
  (notify notifications "Summary" :hints '((:urgency "LOW")))
  (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.Notify" "" 0 "" "Summary" "" nil '((:urgency "LOW")) -1)))

(def-test actions-doesnt-get-sent-when-server-doesnt-support-them (:fixture with-mock-dbus-proxy)
  (notify notifications "Summary" :actions '(:open "Open"))
  (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.Notify" "" 0 "" "Summary" "" nil nil -1)))

(def-test actions-get-sent-when-server-supports-them (:fixture (with-mock-dbus-proxy (mock-dbus-proxy-with-actions-support)))
  (notify notifications "Summary" :actions '("id" "Open"))
  (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.Notify" "" 0 "" "Summary" "" '("id" "Open") nil -1)))

(def-test close-notification (:fixture with-mock-dbus-proxy)
  (let ((sent-notification (notify notifications "Something")))
    (clear sent-notification)
    (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.CloseNotification" (id sent-notification)))))

(def-test update-notifications (:fixture with-mock-dbus-proxy)
  (let ((sent-notification (notify notifications "Something")))
    (update sent-notification :summary "Something else" :body "and a body")
    (is-true (dbus-method-sent dbus-proxy "org.freedesktop.Notifications.Notify" "" (id sent-notification) "" "Something else" "and a body" nil nil -1))
    (is (string= (summary sent-notification) "Something else"))
    (is (string= (body sent-notification) "and a body"))))
