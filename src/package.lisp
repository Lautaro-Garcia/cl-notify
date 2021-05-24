(defpackage :cl-notify
  (:use :cl)
  (:export :send-notification :clear :close-notification :id :update :summary :body :expire-timeout :hints :actions :app-icon
           :markup->string :no-markup :body-notification-markup
           :make-hint :low-urgency-hint :normal-urgency-hint :critical-urgency-hint
           :notifications :notify :server-information :server-capabilities
           :notification-server-info :notification-server-capabilities :name :version :vendor :spec-version
           :standard-dbus-notification-proxy :get-server-information :get-server-capabilities))
