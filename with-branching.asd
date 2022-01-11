;;;; with-branching.asd

(asdf:defsystem #:with-branching
  :description "An implementation of macroexpand-time conditionalization"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivial-indent)
  :components ((:file "with-branching"))
  :in-order-to ((test-op (load-op #:with-branching/test)))
  :perform
  (test-op (o c) (symbol-call '#:with-branching/test '#:test)))

(asdf:defsystem #:with-branching/test
  :description "Tests for WITH-BRANCHING"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:agnostic-lizard)
  :components ((:file "with-branching-test")))
