#lang racket
(require math/statistics)
(require racket/trace)

(define L '(1 2 3 5 6 9 32 1 2 5 12 31 15))

;----METODO SUMA----
(define (sumaSubL l)
    (cond
      ((null? l) 0)
      (else      (+ (car l) (sumaSubL (cdr l))))))
;----SABER CUAL ES EL MAYOR----
(define (obtieneMayorEnLista element lst)
  (cond
    [(or (empty? lst) (<= element (first lst))) (cons element lst)]
    [else (cons (first lst) (obtieneMayorEnLista element (rest lst)))]))


(define (ordenaPorSuma lst)
  (cond
    [(empty? lst) empty]
    [else (obtieneMayorEnLista (first lst) (ordenaPorSuma (rest lst)))]))

;----SUMA LAS LISTAS Y RESULTADO AL LADO----
(define uneIndividuoSuma;
  (lambda (L)
    (map
     (lambda (x)
       (list x (sumaSubL x)))
     L)))
;----DISTRIBUIR LIST----
(define distribuyeCompleto
  (lambda (L k m)
    (cond ((empty? m) (distribuyeCompleto L k (distribuir L k)))
      ((equal?(existeVacia? (individuoVacio? m))#t)(distribuyeCompleto L k (distribuir L k)))
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

(define individuoVacio?
  (lambda (L)
    (map
     (lambda (x)
       (if (empty? x) #t #f))
     L)))

(define existeVacia?
  (lambda(L)
    (if(list? (member #t L))(first(member #t L)) #f)))
;---- ----
;(obtenerSolucion 5000 30 #t 4 L)
(define generacionInicial
  (lambda(individuos L grupos R)
    (cond((zero? individuos) R)
         ((empty? R) ( generacionInicial (- individuos 1) L grupos  (list(uneIndividuoSuma (distribuyeCompleto L grupos '())))))
         (else ( generacionInicial (- individuos 1) L grupos (append R (list (uneIndividuoSuma (distribuyeCompleto L grupos '())))))))))

(define dropSumas
  (lambda (L)
    (map
     (lambda (x)
           (last x))
     L)))

(define parIndividuoDesviacion
  (lambda (L)
    (map
     (lambda (x)
       (list x (stddev(dropSumas x))))
     L)))

(define ordenaDesviacion
  (lambda (x y)
    (< (last x)  (last y))))

(define getOptimo
  (lambda (L)
    (cond((equal? (last(first L)) 0)(car L))
         (else (dropTodasSumas(dropDesviacion L))))))

(define dropDesviacion
  (lambda (L)
    (map
     (lambda (x)
           (remove (last x)(last (remove (last x) x))))
     L)))

(define dropTodasSumas
  (lambda (L)
    (map
     (lambda (x)
      (map
       (lambda ( y)
        (remove (last x)(last(remove (last y) y))))
       x))
     L)))


(define getEnPosicion
  (lambda (l k) (auxEnPosicion l k 0)))



(define auxEnPosicion
  (lambda (l k  n)
    (cond ((= k n) (car l))
          (else (auxEnPosicion (cdr l) k (+ n 1)))
    )
  )
 )
;----Mutacion----

(define ejecutarMutacion
  (lambda (L k m)
    (cond ((empty? m) (ejecutarMutacion L k (mutacion L k)))
      ((equal?(existeVacia? (individuoVacio? m))#t)(ejecutarMutacion L k (mutacion L k)))
          (else m))))

(define mutacion
 (lambda (L k)
   (auxIntercambio L (random k)(random k) '() k )))

(define intercambio
  (lambda ( random i j L R)
    (cond ((empty? R) (intercambio random i j (mutarEnLista L random i j '()) (mutarRestantes L random i j '()) ))
          (else (append L R)))))

(define auxIntercambio
  (lambda (L i j R k)
    (cond ((= i j) (auxIntercambio L i (random k) '() k))
          ((empty? R) (auxIntercambio L i j (intercambio (random (length (getEnPosicion L i))) i j L R) k))
          (else R))))

(define mutarRestantes
  (lambda (L random i j R)
  (cond ((empty? R)( mutarRestantes L random i j (list (cons (getEnPosicion (getEnPosicion L i) random) (getEnPosicion L j)))))
        (else  R))))

(define mutarEnLista
  (lambda (L random i j R)
  (cond ((empty? R)( mutarEnLista L random i j (append (list (remove (getEnPosicion (getEnPosicion L i) random) (getEnPosicion L i)))  (remove (getEnPosicion L j) (remove (getEnPosicion L i) L ) ))))
        (else  R  ))))

(define LL '((1 1) (1) (1)))

(define obtenerSolucion
  (lambda (gens indi elt? gp ls r)
    (cond ((empty? r) (obtenerSolucion (- gens 1) indi elt? gp ls (getOptimo (sort (parIndividuoDesviacion (generacionInicial indi ls gp '())) ordenaDesviacion))))
          ((= 2 (length r)) r)
          ((= 0 gens) (car (sort (parIndividuoDesviacion (preparaProximaGen r)) ordenaDesviacion)))
          (else ( obtenerSolucion (- gens 1) indi elt? gp ls (getOptimo (sort(parIndividuoDesviacion(preparaProximaGen (generarProximaGen indi r gp '() elt?))) ordenaDesviacion)) )))))

(define generarProximaGen
  (lambda (indi l gp r elt?)
    (cond ((empty? l) r)
          ((empty? r)
           (cond ((equal? elt? #t)(generarProximaGen indi (cdr l) gp (list(car l)) elt?))
                 (else (generarProximaGen indi (cdr l) gp (list(ejecutarMutacion (car l) gp '())) elt?))))
          (else (generarProximaGen indi (cdr l) gp (append r (list (ejecutarMutacion (car l) gp '()))) elt?)))))

(define preparaProximaGen
  (lambda (L)
    (map
     (lambda (x)
       (uneIndividuoSuma x))
     L)))

(obtenerSolucion 500 30 #t 4 L '())
