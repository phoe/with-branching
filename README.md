# WITH-COMPILE-TIME-BRANCHING

This is an implementation of compile-time branching in portable Common Lisp.

The main use case is to avoid closing over variables for performance.

## Manual

Let's consider the following function:

```lisp
(defun make-adder (x &key huge-p)
  (lambda (y) (+ x y (if huge-p 1000 0))))
```

The result of calling `(MAKE-ADDER 10)` closes over `HUGE-P` and makes a runtime check for its value.

```lisp
CL-USER> (disassemble (make-adder 10))
; disassembly for (LAMBDA (Y) :IN MAKE-ADDER)
; Size: 65 bytes. Origin: #x53730938                          ; (LAMBDA (Y) :IN MAKE-ADDER)
; 38:       488975F8         MOV [RBP-8], RSI
; 3C:       488BD3           MOV RDX, RBX
; 3F:       E8EC012DFF       CALL #x52A00B30                  ; GENERIC-+
; 44:       488B75F8         MOV RSI, [RBP-8]
; 48:       4881FE17011050   CMP RSI, #x50100117              ; NIL
; 4F:       BFD0070000       MOV EDI, 2000
; 54:       B800000000       MOV EAX, 0
; 59:       480F44F8         CMOVEQ RDI, RAX
; 5D:       E8CE012DFF       CALL #x52A00B30                  ; GENERIC-+
; 62:       488BE5           MOV RSP, RBP
; 65:       F8               CLC
; 66:       5D               POP RBP
; 67:       C3               RET
; 68:       CC10             INT3 16                          ; Invalid argument count trap
; 6A:       6A20             PUSH 32
; 6C:       E8FFFA2CFF       CALL #x52A00470                  ; ALLOC-TRAMP
; 71:       5B               POP RBX
; 72:       E958FFFFFF       JMP #x537308CF
; 77:       CC10             INT3 16                          ; Invalid argument count trap
NIL
```

It would be better for performance if the test was only made once, in `MAKE-ADDER`, rather than on every call of the adder closure. `MAKE-ADDER` could then return one of two functions depending on whether the check succeeds.

```lisp
(defun make-adder (x &key huge-p)
  (if huge-p
      (lambda (y) (+ x y 1000))
      (lambda (y) (+ x y 0))))
```

A brief look at the disassembly of this fixed version shows us that we're right:

```lisp
CL-USER> (disassemble (make-adder 10))
; disassembly for (LAMBDA (Y) :IN MAKE-ADDER)
; Size: 21 bytes. Origin: #x53730BC7                          ; (LAMBDA (Y) :IN MAKE-ADDER)
; C7:       488BD1           MOV RDX, RCX
; CA:       E861FF2CFF       CALL #x52A00B30                  ; GENERIC-+
; CF:       31FF             XOR EDI, EDI
; D1:       E85AFF2CFF       CALL #x52A00B30                  ; GENERIC-+
; D6:       488BE5           MOV RSP, RBP
; D9:       F8               CLC
; DA:       5D               POP RBP
; DB:       C3               RET
NIL
```

Still, with more flags than one, this style of writing code is likely to become unwieldy. For three flags, we would need to write something like this for the runtime version:

```lisp
(defun make-adder (x &key huge-p enormous-p humongous-p)
  (lambda (y) (+ x y
                 (if huge-p 1000 0)
                 (if enormous-p 2000 0)
                 (if humongous-p 3000 0))))
```

But it would look like this for the compile-time version:

```lisp
(defun make-adder (x &key huge-p enormous-p humongous-p)
  (if huge-p
      (if enormous-p
          (if humongous-p
              (lambda (y) (+ x y 1000 2000 3000))
              (lambda (y) (+ x y 1000 2000 0)))
          (if humongous-p
              (lambda (y) (+ x y 1000 0 3000))
              (lambda (y) (+ x y 1000 0 0))))
      (if enormous-p
          (if humongous-p
              (lambda (y) (+ x y 0 2000 3000))
              (lambda (y) (+ x y 0 2000 0)))
          (if humongous-p
              (lambda (y) (+ x y 0 0 3000))
              (lambda (y) (+ x y 0 0 0))))))
```

The total number of combinations for `n` boolean flags is `2^n`, making it hard to write and maintain code with so many branches. This is where `WITH-COMPILE-TIME-BRANCHING` comes into play. Using it, we can write our code in a way that looks similar to the runtime-check version:

```lisp
(defun make-adder (x &key huge-p enormous-p humongous-p)
  (with-compile-time-branching (huge-p enormous-p humongous-p)
    (lambda (y) (+ x y
                   (compile-time-if huge-p 1000 0)
                   (compile-time-if enormous-p 2000 0)
                   (compile-time-if humongous-p 3000 0)))))
```

This code gives us the clarity of runtime-checked version and the performance of a compile-time-checked version. A total of eight versions of the body (and therefore, eight possible `LAMBDA` forms) are generated. At runtime, only one of them is selected, based on the boolean values of the three flags we provided.

Three conditional operators are provided - `COMPILE-TIME-IF`, `COMPILE-TIME-WHEN`, and `COMPILE-TIME-UNLESS`, mimicking the syntax of, respectively, `IF`, `WHEN`, and `UNLESS`.

## Bypassing the compile-time branching

It is possible to use the variable `*COMPILE-TIME-BRANCH-BYPASS*` for bypassing compile-time branching; this is useful e.g. when trying to read the macroexpansions or when debugging. If that variable is set to true, the behavior of the macroexpander is modified:
* `WITH-COMPILE-TIME-BRANCHING` expands into a `PROGN` form,
* `COMPILE-TIME-IF` expands into an `IF` form,
* `COMPILE-TIME-WHEN` expands into a `WHEN` form,
* `COMPILE-TIME-UNLESS` expands into an `UNLESS` form.

## Exceptional situations

Trying to use `COMPILE-TIME-IF`, `COMPILE-TIME-WHEN`, or `COMPILE-TIME-UNLESS` outside the lexical environment established by `WITH-COMPILE-TIME-BRANCHES` will signal a `PROGRAM-ERROR`.

Trying to use a branch name `COMPILE-TIME-IF`, `COMPILE-TIME-WHEN`, or `COMPILE-TIME-UNLESS` that wasn't declared in `WITH-COMPILE-TIME-BRANCHES` will signal a `PROGRAM-ERROR`.

## License

MIT

