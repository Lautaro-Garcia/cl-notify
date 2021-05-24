(in-package :cl-notify/tests)

(def-suite markup)

(in-suite markup)

(def-fixture with-markup-support ()
  (let ((markup (make-instance 'body-notification-markup)))
    (&body)))

(def-fixture with-no-markup-support ()
  (let ((markup (make-instance 'no-markup)))
    (&body)))

(def-test string-body-with-no-server-support (:fixture with-no-markup-support)
  (is (string= "foo" (markup->string markup "foo"))))

(def-test markup-with-no-server-support (:fixture with-no-markup-support)
  (is (string= "bold" (markup->string markup '(:b "bold")))))

(def-test markup-with-no-server-support (:fixture with-no-markup-support)
  (is (string= "regular bold italic underlined hyperlink"
               (markup->string markup '("regular "
                                        (:b "bold ")
                                        (:i "italic ")
                                        (:u "underlined ")
                                        (:br)
                                        (:a "hyperlink" "https://something.com")
                                        (:img "https://something.jpg" "alt"))))))

(def-test markup-with-server-support (:fixture with-markup-support)
  (is (string= "<b>bold</b>" (markup->string markup '(:b "bold")))))

(def-test markup-with-server-support (:fixture with-markup-support)
  (is (string= "regular <b>bold </b><i>italic </i><u>underlined </u><br><a href=\"https://something.com\">hyperlink </a><img src=\"https://something.jpg\" alt=\"alt\"/>"
               (markup->string markup '("regular "
                                        (:b "bold ")
                                        (:i "italic ")
                                        (:u "underlined ")
                                        (:br)
                                        (:a "hyperlink ""https://something.com")
                                        (:img "https://something.jpg" "alt"))))))

(def-test unknown-tag-without-server-support (:fixture with-no-markup-support)
  (is (string= "" (markup->string markup '(:unknown-tag))))
  (is (string= "maybe some text" (markup->string markup '(:unknown-tag "maybe some text")))))

(def-test unknown-tag--server-support (:fixture with-markup-support)
  (is (string= "" (markup->string markup '(:unknown-tag))))
  (is (string= "maybe some text" (markup->string markup '(:unknown-tag "maybe some text")))))
