(load "eMatch.scm")

(define interpret
  (lambda (expr)
    (eMatch expr
            ((+ ,x ,y) (+ x y))
            ((and ,x ,y) (and x (interpret y)))
            (((lambda (,x) ,x) ,y) ((lambda (x) x) (interpret y)))
            (,x x))))


;;; Here's some test code

(display (interpret ((lambda (z) z) 5)))
(printf "\n")
(display (interpret (and #t #t #t #f)))
(printf "\n")
(display (interpret (and #t #t #t)))
(printf "\n")
(display (interpret (+ 1 2)))