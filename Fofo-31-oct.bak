#lang racket

(define L '(30 60 90 25 20 15 16 120 200 43 18 30 30))

(define resolver
  (lambda (generaciones individuos elitismo? grupos ls)
    (primer_elemento_list (last (sort-demo L)) (distribuir ls grupos))))


(define primer_elemento_list
  (lambda(numero_mayor ls)
    (evaluar-funcion-Lisresult ls numero_mayor)
   ))
;-------MEDIR DISTANCIA----------

;---METODO SUMA----------
(define (suma-l l)
    (cond
      ((null? l) 0)
      (else      (+ (car l) (suma-l (cdr l))))))
;----------SABER CUAL ES EL MAYOR----------------------------
(define (insertToSortedList element lst)
  (cond
    [(or (empty? lst) (<= element (first lst))) (cons element lst)]
    [else (cons (first lst) (insertToSortedList element (rest lst)))]))


(define (sort-demo lst)
  (cond
    [(empty? lst) empty]
    [else (insertToSortedList (first lst) (sort-demo (rest lst)))]))

;----------SUMA LAS LISTAS Y RESULTADO AL LADO----------------------------
(define evaluar-funcion-Lisresult;
  (lambda (L numero_mayor)
    (map
     (lambda (x)
       ;(cond (<= (suma-l x) numero_mayor)
       (list x (suma-l x)))
     L)))
;----------DISTRIBUIR LIST------------------
(define distribuir
  (lambda (L k)
    (d1 L k (make-list k '()))))

(define d1
  (lambda(L n R)
    (cond ((empty? L)R)
          (else
           (d1 (cdr L) n (d2 (car L) n R))))))

(define d2
  (lambda( x n R)
    (cond ((zero? (random n))
           (cons(append(car R)(list x))(cdr R)))
          (else
           (cons (car R)(d2 x(- n 1)(cdr R)))))))

(resolver 5000 30 #t 4 L)