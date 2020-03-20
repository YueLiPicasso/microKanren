#lang racket/base

; Based on:
; Jason Hemann and Daniel P. Friedman. 
; microKanren: A Minimal Functional Core for Relational Programming.
; In Proceedings of the 2013 Workshop on Scheme and Functional Programming


; microKanren term := logic variable | pair | (non-variable non-pair) object

(define (var c) (vector c))    ; a Scheme vector represents a logic variable 
(define (var? x) (vector? x))  ; logic variable predicate
(define (var=? x1 x2)          ; logic variable equality
  (= (vector-ref x1 0) (vector-ref x2 0)))
(define (obj=? o1 o2) (eqv? o1 o2)) ; object equality

; unification algorithm

(define (walk u s)
  (let ((pr (and (var? u) (assf (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (unify u v s)
  (let ((u (walk u s))(v (walk v s)))
    (cond
      ((and (var? u)(var? v)(var=? u v)) s)
      ((var? u)(ext-s u v s))
      ((var? v)(ext-s v u s))
      ((and (pair? u)(pair? v))
       (let ((s (unify (car u)(car v) s)))
         (and s (unify (cdr u)(cdr v) s))))
      (else (and (obj=? u v) s)))))

; basic goal constructors

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2)
  (lambda (s/c)
    (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2)
  (lambda (s/c)
    (bind (g1 s/c) g2)))


; list monad (mzero, unit, mplus and bind)

(define mzero '())
(define (unit s/c) (cons s/c mzero))

;; finite depth-first search

#;(define (mplus $1 $2)
    (cond
      ((null? $1) $2)
      (else (cons (car $1) (mplus (cdr $1) $2)))))

#;(define (bind $ g)
    (cond
      ((null? $) mzero)
      (else (mplus (g (car $)) (bind (cdr $) g)))))

;; infinite depth-first search

#;(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1)(delay (mplus (force $1) $2)))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (delay (bind (force $) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;; infinite interleaved search

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1)(delay (mplus $2 (force $1)))) ; swap here
    (else (cons (car $1) (mplus $2 (cdr $1))))))   ; swap here


; stream techniques highlight

(define-syntax delay ; suspension
  (syntax-rules ()
    ((delay e) (lambda () e))))

(define-syntax snooze ; the inverse-eta-delay goal constructor
  (syntax-rules ()
    ((snooze g)(lambda (s/c) (delay (g s/c))))))

(define-syntax force ; execute a delayed procedure
  (syntax-rules ()
    ((force g) (g))))

; Note that:
; if we use "define" instead of "define-syntax"
; as follows, the we will not achieve the desired
; effect since Scheme is call-by-value
#;(define (force g) (g)) 
#;(define (delay e) (lambda () e))
#;(define (snooze g) (lambda (s/c) (delay (g s/c))))



; auxiliaries

(define empty-state '(() . 0))

(define (take-inf n s-inf)
    (cond
      ((zero? n) '())
      ((null? s-inf) '())
      ((pair? s-inf)(cons (car s-inf) (take-inf (- n 1) (cdr s-inf))))
      ((procedure? s-inf)(take-inf n (force s-inf)))))

; tests

#;(let ((goal 
       (call/fresh
        (lambda (x)
          (call/fresh
           (lambda (y)
             (call/fresh
              (lambda (z)
                (== `(,x atom (,z)) `(air ,y (plane)))))))))))
  (goal '(() . 0)))


(define (fives x) (disj (== x 'five)(snooze (fives x))))
(define (sixes x) (disj (== x 'six)(snooze (sixes x))))
(define (fives&sixes x) (disj (fives x) (sixes x)))

#;((call/fresh fives) empty-state)
#;(take-inf 1 ((call/fresh fives) empty-state))
#;(take-inf 2 ((call/fresh fives) empty-state))
#;(take-inf 10 ((call/fresh fives) empty-state))

((call/fresh fives&sixes) empty-state)
(take-inf 1 ((call/fresh fives&sixes) empty-state))
(take-inf 2 ((call/fresh fives&sixes) empty-state))
(take-inf 10 ((call/fresh fives&sixes) empty-state))