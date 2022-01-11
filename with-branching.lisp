;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WITH-BRANCHING
;;;; © Michał "phoe" Herda 2022
;;;; License: MIT

(defpackage #:with-branching
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:i #:trivial-indent))
  (:export #:*branch-bypass*
           #:with-branching
           #:branch-if
           #:branch-when
           #:branch-unless))

(in-package #:with-branching)

(i:define-indentation branch-if (4 4 4))
(i:define-indentation branch-when (as when))
(i:define-indentation branch-unless (as unless))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementation

(defvar *branch-bypass* nil)

(define-symbol-macro %in-branching% nil)

(define-symbol-macro %all-branches% ())

(defmacro with-branching ((&rest branches) &body body
                                           &environment env)
  (cond (*branch-bypass*
         `(progn ,@body))
        (t (let ((all-branches (macroexpand-1 '%all-branches% env)))
             `(symbol-macrolet ((%in-branching% t)
                                (%all-branches% (,@branches ,@all-branches))
                                (%true-branches% ()))
                (%with-branching ,branches ,@body))))))

(defmacro %with-branching ((&rest branches) &body body
                                            &environment env)
  (cond ((not (null branches))
         (destructuring-bind (branch . other-branches) branches
           (let ((true-branches (macroexpand-1 '%true-branches% env)))
             `(if ,branch
                  (symbol-macrolet
                      ((%true-branches% (,branch . ,true-branches)))
                    (%with-branching (,@other-branches)
                      ,@body))
                  (%with-branching (,@other-branches)
                    ,@body)))))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defun conditional-error (name)
  `(a:simple-program-error
    "~A must be used inside the lexical scope established by ~
     WITH-BRANCHING."
    ',name))

(defun missing-branch (name)
  `(a:simple-program-error
    "The macroexpand-time branch ~S was not defined in any encloding
     WITH-BRANCHING form."
    ',name))

(defmacro branch-if (branch then &optional else &environment env)
  (cond (*branch-bypass*
         `(if ,branch ,then ,else))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (conditional-error 'if))
        ((member branch (macroexpand-1 '%true-branches% env))
         then)
        (t (or else `(progn)))))

(defmacro branch-when (branch &body body &environment env)
  (cond (*branch-bypass*
         `(when ,branch ,@body))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (conditional-error 'when))
        ((not (member branch (macroexpand-1 '%true-branches% env)))
         `(progn))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defmacro branch-unless (branch &body body &environment env)
  (cond (*branch-bypass*
         `(unless ,branch ,@body))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (conditional-error 'unless))
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
 (documentation '*branch-bypass* 'variable)
 "Bypasses macroexpand-time branching. The bypass inhibits all macroexpand-time
branching and instead defers all checks in expanded code to runtime in the
following manner:
\
* WITH-BRANCHING -> PROGN
* BRANCH-IF -> IF
* BRANCH-WHEN -> WHEN
* BRANCH-UNLESS -> UNLESS"
 (documentation 'with-branching 'function)
 "Establishes a lexical environment in which it is possible to use
macroexpand-time branching. Within the lexical scope of
WITH-BRANCHING, it is possible to use BRANCH-IF,
BRANCH-WHEN, and BRANCH-UNLESS to conditionalize whether
some forms are included at compilation time.
\
The first argument must be a list of symbols which name variables. This macro
will expand into a series of conditionals"
 (documentation 'branch-if 'function)
 "Chooses between the forms to include based on whether a macroexpand-time
branch is true. The first argument must be a symbol naming a branch in the
lexically enclosing WITH-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-BRANCHES."
 (documentation 'branch-when 'function)
 "Includes some forms based on whether a macroexpand-time branch is true. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-BRANCHES."
 (documentation 'branch-unless 'function)
 "Includes some forms based on whether a macroexpand-time branch is false. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-BRANCHING form.
\
It is an error to use this macro outside the lexical environment established by
WITH-BRANCHES.")
