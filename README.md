# Show notifications in your desktop

Common lisp library to send notifications to your desktop through D-Bus (kinda as a `libnotify` replacement).

It follows the [Desktop Notification Specification](https://developer.gnome.org/notification-spec)

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
                   :body '("Look at " (:u "this image ") (:br)
                           (:img "file://home/user/lisp.png" "Common lisp") (:br)
                           (:a "https://github.com/Lautaro-Garcia/cl-notify" "click this link to see more info")))

;; Arbitrary hints (you should check if your implementation actually uses them)
(send-notification "A summary"
                   :hints (list (make-hint "action-icons" '(:boolean) t))
                   :actions '("id" "email"))
```

## Listen for events (notification closes or buttons were pushed)
```common-lisp
;; The signal handling loop starts in the background when you define a callback.
(define-callback my-callback :action (notification-id action-id)
  (format t "The action ~a in the notification ~a was clicked" action-id notification-id))

(define-callback my-callback :close (notification-id reason)
  (format t "The notification ~a was closed! The reason was ~a" notification-id reason))

;; You could do the same without using the macro
(register-callback :close 'my-callback
                   (lambda (notification-id reason)
                     (format t "The notification ~a was closed! The reason was ~a" notification-id reason))))

;; You can unregister a callback by its name and type
(unregister-callback :close 'my-callback)

;; You can stop handling signals (this also deletes every callback)
(stop-signal-handling-loop)
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

## Further documentation
You should probably read the [Desktop Notification Specification](https://developer.gnome.org/notification-spec) if you'd like to know
the actions, signals and values available in this library.
