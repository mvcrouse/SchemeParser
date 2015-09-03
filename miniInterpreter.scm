(load "eMatch.scm")

(define interpret
  (lambda (expr)
    (eMatch expr
            ((+ ,x ,y) (+ x y))
            ((and ,x ,y ,z ***) (and (interpret x) (interpret (cons 'and (cons y z)))))
            ((and ,x ,y) (and (interpret x) (interpret y)))
            (((lambda (,x) ,x) ,y) ((lambda (x) x) (interpret y)))
            (,x x))))


;;; Here's some test code

(display (interpret '((lambda (z) z) 5)))
(printf "\n")
(display (interpret '(and #t #t #f #t #t #t)))
(printf "\n")
(display (interpret '(and #t 5)))
(printf "\n")
(display (interpret '(+ 1 2)))