#lang racket

(require "thingy.rkt")

(define (compile-simpl sexp)
  (make-primpl sexp)
  )

(define (make-primpl sexp)
  (match sexp
    [_ #:when (and (list? sexp) (not(empty? sexp)) (equal? 'vars (first sexp))) (begin (define sp (gensym)) (define instr (make-primpl (cons 'seq (rest (rest sexp)))))
                                (cons (set '(0 sp) sp) (append instr (cons (list 'halt)(definitions (first (rest sexp)))) (list (list 'data sp sp)))))]
    [`(print ,var) (if (string? var) (list 'print-string var)
                              (append (redefine (make-primpl var)) (list (list 'print-val '(0 sp))))
                                      )]
    [`(seq ,stmt1 ..2) (begin (define first (redefine (make-primpl (car stmt1))))
                                 (define second (redefine (make-primpl (cons 'seq (rest(rest sexp))))))
                                 (append first second)
                                 )]
    [`(seq ,stmt)   (make-primpl stmt)]
    [`(+ ,var1 ,var2) (append  (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1) (list 'add '(0 sp) '(0 sp) '(1 sp))))]
    [`(- ,var1 ,var2) (append (list (list 'add 'sp 'sp 1))(make-primpl var1) (list (list 'move '(-1 sp) '(0 sp)))
                              (make-primpl var2) (list (list 'sub 'sp 'sp 1)) (list (list 'sub '(0 sp) '(0 sp) '(1 sp) )))]
    [`(* ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'mul '(0 sp) '(0 sp) '(1 sp) )))]
    [`(div ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'div '(0 sp) '(0 sp) '(1 sp))))]
    [`(mod ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'mod '(0 sp) '(0 sp) '(1 sp) )))]
    [`(= ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'equal '(0 sp) '(0 sp) '(1 sp) )))]
    [`(> ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'gt '(0 sp) '(0 sp) '(1 sp) )))]
    [`(< ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'lt '(0 sp) '(0 sp) '(1 sp) )))]
    [`(>= ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'ge '(0 sp) '(0 sp) '(1 sp) )))]
    [`(<= ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'le '(0 sp) '(0 sp) '(1 sp) )))]
    [`(and ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'land '(0 sp) '(0 sp) '(1 sp) )))]
    [`(or ,var1 ,var2) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl var1)) (list (list 'move '(-1 sp) '(0 sp)))
                              (redefine (make-primpl var2)) (list (list 'sub 'sp 'sp 1)) (list (list 'lor '(0 sp) '(0 sp) '(1 sp) )))]
    [`(not ,bexp) (append (list (list 'add 'sp 'sp 1)) (redefine (make-primpl bexp)) (list (list 'sub 'sp 'sp 1)) (list (list 'lnot '(0 sp) '(1 sp))))]
    [`(set ,id ,exp)  (append (redefine (make-primpl exp)) (list (list 'move id '(0 sp))))]
    [`(iif ,bexp ,stmt1 ,stmt2) (begin
                                  (define label1 (gensym))
                                  (define label2 (gensym))
                                  (define label3 (gensym))
                                  (define beval (redefine (make-primpl bexp)))
                                  (define firstOp (redefine (make-primpl stmt1)))
                                  (define secondOp (redefine (make-primpl stmt2)))
                                  (append beval (list (list 'branch '(0 sp) label1)) (list (list 'jump label2)) (list (list 'label label1)) firstOp
                                                  (list (list 'jump label3)) (list (list 'label label2)) secondOp (list (list 'label label3))))]
    [`(while ,bexp ,stmt1 ...) (begin (define label1 (gensym))
                                      (define label2 (gensym))
                                      (define label3 (gensym))
                                      (define sequence (redefine (make-primpl (cons 'seq (rest(rest sexp))))))
                                      (define beval (redefine (make-primpl bexp)))
                                      (define beval2 (redefine beval))
                                     (append (list (list 'label label1)) beval2 (list (list 'branch '(0 sp) label2)) (list (list 'jump label3))
                                                     (list (list 'label label2)) sequence (list (list 'jump label1)) (list (list 'label label3))))]
    ['true (list 'move '(0 sp) #t)]
    ['false (list 'move '(0 sp) #f)]
    [`(skip)  empty]
    [_  (list 'move '(0 sp) sexp)]
    )
  )

(define (redefine lst) (cond
                        [(empty? lst) lst]
                        [(list?(first lst)) lst]
                        [else (list lst)]
                       ))

(define (definitions vars) (foldl
                            (lambda (in lst) (append lst (list(append (list 'data) in))))
                            empty
                            vars))


(define testing (compile-simpl 
 '(vars ([i 0] [j 10])
     (set i (+ (* j x) y)) (print i))
))

testing
(define test-prog (primpl-assemble '((add sp sp 1)
  (jump (3))
  (data i 5)
  (add sp sp 1)
  (add sp sp 1)
  (move (0 sp) j)
  (move (-1 sp) (0 sp))
  (move (0 sp) x)
  (sub sp sp 1)
  (mul (0 sp) (0 sp) (1 sp))
  (move (-1 sp) (0 sp))
  (move (0 sp) y)
  (sub sp sp 1)
  (add (0 sp) (0 sp) (1 sp))
  (move i (0 sp))
  (move (0 sp) i)
  (print-val (0 sp))
  (halt)
  (data i 0)
  (data j 10)
  (data sp sp))))

;(load-primp test-prog)
;(run-primp)