;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WITH-MACROEXPAND-TIME-BRANCHING
;;;; © Michał "phoe" Herda 2022
;;;; License: MIT

(defpackage #:with-macroexpand-time-branching
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:i #:trivial-indent))
  (:export #:*macroexpand-time-branch-bypass*
           #:with-macroexpand-time-branching
           #:macroexpand-time-if
           #:macroexpand-time-when
           #:macroexpand-time-unless))

(in-package #:with-macroexpand-time-branching)

(i:define-indentation macroexpand-time-if (4 4 4))
(i:define-indentation macroexpand-time-when (as when))
(i:define-indentation macroexpand-time-unless (as unless))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementation

(defvar *macroexpand-time-branch-bypass* nil)

(define-symbol-macro %in-branching% nil)

(define-symbol-macro %all-branches% ())

(defmacro with-macroexpand-time-branching ((&rest branches) &body body
                                           &environment env)
  (cond (*macroexpand-time-branch-bypass*
         `(progn ,@body))
        (t (let ((all-branches (macroexpand-1 '%all-branches% env)))
             `(symbol-macrolet ((%in-branching% t)
                                (%all-branches% (,@branches ,@all-branches))
                                (%true-branches% ()))
                (%with-macroexpand-time-branching ,branches ,@body))))))

(defmacro %with-macroexpand-time-branching ((&rest branches) &body body
                                            &environment env)
  (cond ((not (null branches))
         (destructuring-bind (branch . other-branches) branches
           (let ((true-branches (macroexpand-1 '%true-branches% env)))
             `(if ,branch
                  (symbol-macrolet
                      ((%true-branches% (,branch . ,true-branches)))
                    (%with-macroexpand-time-branching (,@other-branches)
                      ,@body))
                  (%with-macroexpand-time-branching (,@other-branches)
                    ,@body)))))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defun macroexpand-time-conditional-error (name)
  `(a:simple-program-error
    "~A must be used inside the lexical scope established by ~
     WITH-MACROEXPAND-TIME-BRANCHING."
    ',name))

(defun macroexpand-time-missing-branch (name)
  `(a:simple-program-error
    "The macroexpand-time branch ~S was not defined in any encloding
     WITH-MACROEXPAND-TIME-BRANCHING form."
    ',name))

(defmacro macroexpand-time-if (branch then &optional else &environment env)
  (cond (*macroexpand-time-branch-bypass*
         `(if ,branch ,then ,else))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (macroexpand-time-missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (macroexpand-time-conditional-error 'macroexpand-time-if))
        ((member branch (macroexpand-1 '%true-branches% env))
         then)
        (t (or else `(progn)))))

(defmacro macroexpand-time-when (branch &body body &environment env)
  (cond (*macroexpand-time-branch-bypass*
         `(when ,branch ,@body))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (macroexpand-time-missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (macroexpand-time-conditional-error 'macroexpand-time-when))
        ((not (member branch (macroexpand-1 '%true-branches% env)))
         `(progn))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defmacro macroexpand-time-unless (branch &body body &environment env)
  (cond (*macroexpand-time-branch-bypass*
         `(unless ,branch ,@body))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (macroexpand-time-missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (macroexpand-time-conditional-error 'macroexpand-time-unless))
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
 (documentation '*macroexpand-time-branch-bypass* 'variable)
 "Bypasses macroexpand-time branching. The bypass inhibits all macroexpand-time
branching and instead defers all checks in expanded code to runtime in the
following manner:
\
* WITH-MACROEXPAND-TIME-BRANCHING -> PROGN
* MACROEXPAND-TIME-IF -> IF
* MACROEXPAND-TIME-WHEN -> WHEN
* MACROEXPAND-TIME-UNLESS -> UNLESS"
 (documentation 'with-macroexpand-time-branching 'function)
 "Establishes a lexical environment in which it is possible to use
macroexpand-time branching. Within the lexical scope of
WITH-MACROEXPAND-TIME-BRANCHING, it is possible to use MACROEXPAND-TIME-IF,
MACROEXPAND-TIME-WHEN, and MACROEXPAND-TIME-UNLESS to conditionalize whether
some forms are included at compilation time.
\
The first argument must be a list of symbols which name variables. This macro
will expand into a series of conditionals"
 (documentation 'macroexpand-time-if 'function)
 "Chooses between the forms to include based on whether a macroexpand-time
branch is true. The first argument must be a symbol naming a branch in the
lexically enclosing WITH-MACROEXPAND-TIME-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-MACROEXPAND-TIME-BRANCHES."
 (documentation 'macroexpand-time-when 'function)
 "Includes some forms based on whether a macroexpand-time branch is true. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-MACROEXPAND-TIME-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-MACROEXPAND-TIME-BRANCHES."
 (documentation 'macroexpand-time-unless 'function)
 "Includes some forms based on whether a macroexpand-time branch is false. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-MACROEXPAND-TIME-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-MACROEXPAND-TIME-BRANCHES.")
