(asdf:defsystem :cl-notify
  :description "Lisp libnotify bindings"
  :author "Lautaro García"
  :version "1.0.0"
  :serial t
  :depends-on (:dbus :stmx :bordeaux-threads)
  :in-order-to ((test-op (test-op :cl-notify/tests)))
  :pathname "src"
  :components ((:file "package")
               (:file "markup")
               (:file "hints")
               (:file "dbus-protocol")
               (:file "notifications")
               (:file "close-notification-reason")
               (:file "signals")))

(asdf:defsystem :cl-notify/tests
  :description "cl-notify tests"
  :author "Lautaro García"
  :version "1.0.0"
  :depends-on (:cl-notify :fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "dbus-notification-proxy-mock")
               (:file "markup")
               (:file "notifications"))
  :perform (test-op (op s) (uiop:symbol-call :5am :run-all-tests)))
