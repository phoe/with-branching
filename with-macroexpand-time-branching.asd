;;;; with-macroexpand-time-branching.asd

(asdf:defsystem #:with-macroexpand-time-branching
  :description "An implementation of macroexpand-time conditionalization"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivial-indent)
  :components ((:file "with-macroexpand-time-branching"))
  :in-order-to ((test-op (load-op #:with-macroexpand-time-branching/test)))
  :perform
  (test-op (o c) (symbol-call '#:with-macroexpand-time-branching/test '#:test)))

(asdf:defsystem #:with-macroexpand-time-branching/test
  :description "Tests for WITH-MACROEXPAND-TIME-BRANCHING"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:agnostic-lizard)
  :components ((:file "with-macroexpand-time-branching-test")))
