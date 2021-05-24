# Show notifications in your desktop

Common lisp library to send notifications to your desktop through D-Bus (kinda as a `libnotify` replacement)

## Examples
```common-lisp
;; Send a basic notification
(send-notification "Something" :app-name "My application" :timeout-in-millis 10000)

;; Close notification
(clear *)

;; Send a notification with actions (buttons)
(send-notification "With buttons" :actions '("id1" "Do this!" "id2" "Do this other thing!"))

;; Update previous notification to add a body and an urgency level
(update * :body '(:b "If your notification server supports this, this will be bold!")
          :hints (list (critical-urgency-hint)))

;; A more complex body
(send-notification "This is the title"
                   :app-icon "media-record"
                   :body '("Look at " (:u "this image ") (:br) (:img "file://home/user/lisp.png" "Common lisp") (:br) (:a "https://github.com/Lautaro-Garcia/cl-notify" "click this link to see more info")))
```

## Get information about your notification server
```common-lisp
(notification-server-info) ;; => #<SERVER-INFORMATION name="Plasma" version="5.21.5" vendor="5.21.5" spec-version="1.2">
(notification-server-capabilities) ;; => ("body" "body-hyperlinks" "body-markup" "body-images" "icon-static" "actions")
```

## Requirements
You'll need to have `libfixposix` installed in your system in order to use this library (it's a [dbus](https://github.com/death/dbus) dependency)

## Running the tests
```common-lisp
(asdf:test-system :cl-notify)
```
