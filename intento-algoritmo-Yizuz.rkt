#lang racket
(define L '(1 25 13 12))
(define generacion 0)
(define possible-fitness (combinations L 2)) ; generacion 1

(define sumaTotal
  (lambda (lista acum)
    (if (empty? lista) acum 
        (sumaTotal (cdr lista) (+ acum (car lista))))))
(sumaTotal L 0)

(define fitness
  (lambda (lista _subListas)
    (/ (sumaTotal lista 0) _subListas)))

(fitness L 3)

(define listaAux
  (lambda (lista resultante)
    (if (empty? lista) resultante
        (listaAux (cdr lista) (cons (- (fitness L 3) (sumaTotal (car lista) 0)) resultante)))))
(listaAux (combinations L 2) '())

(define mutar
  (lambda (lista)
    (if (empty? lista) lista
        (append (cdr lista) (random (argmax max L))))))
        
(mutar (listaAux (combinations L 2) '()))  
