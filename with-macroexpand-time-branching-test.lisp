;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WITH-MACROEXPAND-TIME-BRANCHING
;;;; © Michał "phoe" Herda 2022
;;;; License: MIT

(defpackage #:with-macroexpand-time-branching/test
  (:use #:cl)
  (:local-nicknames (#:a #:agnostic-lizard)
                    (#:w #:with-macroexpand-time-branching))
  (:export #:test))

(in-package #:with-macroexpand-time-branching/test)

(defun test-values-1 (x y z)
  (let ((result 0))
    (w:with-macroexpand-time-branching (x y z)
      (w:macroexpand-time-when x
        (incf result 100))
      (w:macroexpand-time-unless y
        (incf result 20))
      (w:macroexpand-time-if z
          (incf result 3)
          (incf result 4)))
    result))

(defun test-values-2 (x y z)
  (let ((result 0))
    (when x
      (incf result 100))
    (unless y
      (incf result 20))
    (if z
        (incf result 3)
        (incf result 4))
    result))

(defun test-values ()
  (dolist (x '(nil t))
    (dolist (y '(nil t))
      (dolist (z '(nil t))
        (let ((actual (test-values-1 x y z))
              (expected (test-values-2 x y z)))
          (assert (= actual expected)))))))

(defparameter *expansion-before*
  `(w:with-macroexpand-time-branching (x y z)
     (w:macroexpand-time-when x
       (print "X is true"))
     (w:macroexpand-time-unless y
       (print "X is false"))
     (w:macroexpand-time-if z
         (print "Z is true!")
         (print "Z is false!"))))

(defparameter *expansion-after*
  `(symbol-macrolet ((w::%in-branching% t)
                     (w::%all-branches% (x y z))
                     (w::%true-branches% nil))
     (if x
         (symbol-macrolet ((w::%true-branches% (x)))
           (if y
               (symbol-macrolet ((w::%true-branches% (y x)))
                 (if z
                     (symbol-macrolet ((w::%true-branches% (z y x)))
                       (progn (print "X is true") (progn) (print "Z is true!")))
                     (progn (print "X is true") (progn) (print "Z is false!"))))
               (if z
                   (symbol-macrolet ((w::%true-branches% (z x)))
                     (progn
                       (print "X is true")
                       (print "X is false")
                       (print "Z is true!")))
                   (progn
                     (print "X is true")
                     (print "X is false")
                     (print "Z is false!")))))
         (if y
             (symbol-macrolet ((w::%true-branches% (y)))
               (if z
                   (symbol-macrolet ((w::%true-branches% (z y)))
                     (progn (progn) (progn) (print "Z is true!")))
                   (progn (progn) (progn) (print "Z is false!"))))
             (if z
                 (symbol-macrolet ((w::%true-branches% (z)))
                   (progn (progn) (print "X is false") (print "Z is true!")))
                 (progn (progn) (print "X is false") (print "Z is false!")))))))

(defun test-expansion ()
  (let* ((form *expansion-before*)
         (expected *expansion-after*)
         (actual (a:macroexpand-all form)))
    (assert (equal expected actual))))

(defun test-missing-lexical-environment ()
  (let ((x 42))
    (declare (ignorable x))
    (flet ((test-1 () (w:macroexpand-time-if x :foo :bar))
           (test-2 () (w:macroexpand-time-when x :foo))
           (test-3 () (w:macroexpand-time-unless x :foo))
           (test (fn)
             (multiple-value-bind (value error)
                 (ignore-errors (funcall fn))
               (check-type value null)
               (check-type error program-error))))
      (test #'test-1)
      (test #'test-2)
      (test #'test-3))))

(defun test-missing-branch ()
  (let ((x 42) (y 24))
    (declare (ignorable x y))
    (flet ((test ()
             (w:with-macroexpand-time-branching (x)
               (w:macroexpand-time-if y 42))))
      (multiple-value-bind (value error)
          (ignore-errors (funcall #'test))
        (check-type value null)
        (check-type error program-error)))))

(defun test ()
  (test-values)
  (test-expansion)
  (test-missing-lexical-environment)
  (test-missing-branch)
  t)
