;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WITH-COMPILE-TIME-BRANCHING
;;;; © Michał "phoe" Herda 2022
;;;; License: MIT

(defpackage #:with-compile-time-branching
  (:use #:cl)
  (:local-nicknames (#:i #:trivial-indent))
  (:export #:*compile-time-branch-bypass*
           #:with-compile-time-branching
           #:compile-time-if
           #:compile-time-when
           #:compile-time-unless))

(in-package #:with-compile-time-branching)

(i:define-indentation compile-time-if (4 4 4))
(i:define-indentation compile-time-when (as when))
(i:define-indentation compile-time-unless (as unless))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementation

(defvar *compile-time-branch-bypass* nil)

(define-symbol-macro %in-branching% nil)

(defmacro with-compile-time-branching ((&rest branches) &body body)
  (cond (*compile-time-branch-bypass*
         `(progn ,@body))
        (t `(symbol-macrolet ((%in-branching% t)
                              (%true-branches% ()))
              (%with-compile-time-branching ,branches ,@body)))))

(defmacro %with-compile-time-branching ((&rest branches) &body body
                                        &environment env)
  (cond ((not (null branches))
         (destructuring-bind (branch . other-branches) branches
           (let ((true-branches (macroexpand-1 '%true-branches% env)))
             `(if ,branch
                  (symbol-macrolet
                      ((%true-branches% (,branch . ,true-branches)))
                    (%with-compile-time-branching (,@other-branches) ,@body))
                  (%with-compile-time-branching (,@other-branches) ,@body)))))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defun compile-time-conditional-error (name)
  `(error "~A must be used inside the lexical scope ~
           established by WITH-COMPILE-TIME-BRANCHING."
          ',name))

(defmacro compile-time-if (branch then &optional else &environment env)
  (cond (*compile-time-branch-bypass*
         `(if ,branch ,then ,else))
        ((not (macroexpand-1 '%in-branching% env))
         (compile-time-conditional-error 'compile-time-if))
        ((member branch (macroexpand-1 '%true-branches% env))
         then)
        (t (or else `(progn)))))

(defmacro compile-time-when (branch &body body &environment env)
  (cond (*compile-time-branch-bypass*
         `(when ,branch ,@body))
        ((not (macroexpand-1 '%in-branching% env))
         (compile-time-conditional-error 'compile-time-when))
        ((not (member branch (macroexpand-1 '%true-branches% env)))
         `(progn))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defmacro compile-time-unless (branch &body body &environment env)
  (cond (*compile-time-branch-bypass*
         `(unless ,branch ,@body))
        ((not (macroexpand-1 '%in-branching% env))
         (compile-time-conditional-error 'compile-time-unless))
        ((member branch (macroexpand-1 '%true-branches% env))
         `(progn))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Documentation

(setf
 (documentation '*compile-time-branch-bypass* 'variable)
 "Bypasses compile-time branching. The bypass inhibits all compile-time
branching and instead defers all checks in expanded code to runtime in the
following manner:
\
* WITH-COMPILE-TIME-BRANCHING -> PROGN
* COMPILE-TIME-IF -> IF
* COMPILE-TIME-WHEN -> WHEN
* COMPILE-TIME-UNLESS -> UNLESS"
 (documentation 'with-compile-time-branching 'function)
 "Establishes a lexical environment in which it is possible to use compile-time
branching. Within the lexical scope of WITH-COMPILE-TIME-BRANCHING, it is
possible to use COMPILE-TIME-IF, COMPILE-TIME-WHEN, and COMPILE-TIME-UNLESS to
conditionalize whether some forms are included at compilation time.
\
The first argument must be a list of symbols which name variables. This macro
will expand into a series of conditionals"
 (documentation 'compile-time-if 'function)
 "Chooses between the forms to include based on whether a compile-time branch is
true. The first argument must be a symbol naming a branch in the lexically
enclosing WITH-COMPILE-TIME-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-COMPILE-TIME-BRANCHES."
 (documentation 'compile-time-when 'function)
 "Includes some forms based on whether a compile-time branch is true. The first
argument must be a symbol naming a branch in the lexically enclosing
WITH-COMPILE-TIME-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-COMPILE-TIME-BRANCHES."
 (documentation 'compile-time-unless 'function)
 "Includes some forms based on whether a compile-time branch is false. The first
argument must be a symbol naming a branch in the lexically enclosing
WITH-COMPILE-TIME-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-COMPILE-TIME-BRANCHES.")
