#lang racket
(require math/statistics)

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
  (lambda (L)
    (map
     (lambda (x)
       (list x (suma-l x)))
     L)))
;----------DISTRIBUIR LIST------------------
(define distribuirPapi
  (lambda (L k m)
    (cond ((empty? m) (distribuirPapi L k (distribuir L k)))
      ((equal?(retornaSiVacia (verificarListaVacia m))#t)(distribuirPapi L k (distribuir L k)))
          (else m))))

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

(define verificarListaVacia
  (lambda (L)
    (map
     (lambda (x)
       (if (empty? x) #t #f))
     L)))

(define retornaSiVacia
  (lambda(L)
    (if(list? (member #t L))(first(member #t L)) #f)))
;-------------------------------------------------
;(resolver 5000 30 #t 4 L)
(define crear-primera-gen
  (lambda(individuos L grupos R)
    (cond((zero? individuos) R)
         ((empty? R) ( crear-primera-gen (- individuos 1) L grupos  (list(evaluar-funcion-Lisresult (distribuirPapi L grupos '())))))
         (else ( crear-primera-gen (- individuos 1) L grupos (append R (list (evaluar-funcion-Lisresult (distribuirPapi L grupos '())))))))))

;( crear-primera-gen 2 L 4 '())
;(verificarListaVacia '((2 1 5)()(4 0 4)))
;(distribuirPapi L 4 '())
;(suma-l ( crear-primera-gen 2 L 4 '()))
(define separar-sumas
  (lambda (L)
    (map
     (lambda (x)
           (last x))
     L)))

(define unificarGen-suma
  (lambda (L)
    (map
     (lambda (x)
       (list x (stddev(separar-sumas x))))
     L)))

;(crear-primera-gen 1 L 4 '())
;(separar-sumas (crear-primera-gen 1 L 4 '())) ;Suma la info

(define sortlastfirst
  (lambda (x y)
    (< (last x)  (last y))))

(define devuelvePerfecto
  (lambda (L)
    (cond((equal? (last(first L)) 0)(car L))
         (else (clearSuma(clearVarianza L))))))

(define clearVarianza
  (lambda (L)
    (map
     (lambda (x)
           (remove (last x)(last (remove (last x) x))))
     L)))

(define clearSuma
  (lambda (L)
    (map
     (lambda (x)
      (map
       (lambda ( y)
        (remove (last x)(last(remove (last y) y))))
       x))
     L)))

(devuelvePerfecto (sort (unificarGen-suma (crear-primera-gen 3 L 3 '())) sortlastfirst))
;(sort (unificarGen-suma (crear-primera-gen 10 L 4 '())) sortlastfirst)
;(unificarGen-suma (crear-primera-gen 2 L 4 '()))