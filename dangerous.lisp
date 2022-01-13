;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WITH-BRANCHING
;;;; © Michał "phoe" Herda 2022
;;;; License: MIT

(defpackage #:with-branching/dangerous
  (:use #:cl)
  (:shadow #:with-branching #:if #:when #:unless)
  (:local-nicknames (#:w #:with-branching))
  (:export #:with-branching #:if #:when #:unless))

(in-package #:with-branching/dangerous)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-branching) (macro-function 'w:with-branching)
        (macro-function 'if) (macro-function 'w:branch-if)
        (macro-function 'when) (macro-function 'w:branch-when)
        (macro-function 'unless) (macro-function 'w:branch-unless)))
