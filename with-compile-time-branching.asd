;;;; with-compile-time-branching.asd

(asdf:defsystem #:with-compile-time-branching
  :description "An implementation of compile-time conditionalization"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-indent)
  :components ((:file "with-compile-time-branching"))
  :in-order-to ((test-op (load-op #:with-compile-time-branching/test)))
  :perform
  (test-op (o c) (symbol-call '#:with-compile-time-branching/test '#:test)))

(asdf:defsystem #:with-compile-time-branching/test
  :description "Tests for WITH-COMPILE-TIME-BRANCHING"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:agnostic-lizard)
  :components ((:file "with-compile-time-branching-test")))
