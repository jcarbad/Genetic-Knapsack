#lang racket

(define L '(10 60 90 25 20 15 16 120 200 43 18 40 30))
(define _k 3)
(define _individuo '((30 60 90) ( 25 20 15 16 30) (200 43) (18 30 30)))
(define _poblacion 30)
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

;-------SUMA TOTAL DE LISTA ------------
; EJ: (1 2 3 25) => 31
(define suma-lista
  (lambda (L acum)
    (if (empty? L) acum
        (suma-lista (cdr L) (+ acum (car L))))))

;----- SUMA TOTAL DE INDIVIDUO (para promedio) ------
;EJ: ((5 7) + (3 8) + (4 6)) => [12] [11] [10] => 33
(define suma-individuo
  (lambda (I acum)
    (if (empty? I) acum
        (suma-individuo (cdr I) (+ acum (suma-lista (car I) 0))))))

;----- Promedio de Individuo --------
;Ej: ((5 7) (3 8) (4 6)) => 33/3 => 11
(define promedio-individuo
  (lambda (I)
    (if (empty? I) 0
        (/ (suma-individuo I 0) (length I)))))

;----- Cuadrado de diferencia del promedio de individuo --------
;Ej: X-Xi
(define distancia-de-promedio
  (lambda (L prom)
    (expt (- (suma-lista L 0) prom) 2)))

;----- Suma de cuadrados de distancias ---------
(define suma-cuadrados-distancia
  (lambda (I acum _promedio)
    (if (empty? I) acum
        (suma-cuadrados-distancia (cdr I) (+ acum (distancia-de-promedio (car I) _promedio)) _promedio))))

;------ Varianza de un individuo ---------
(define varianza-individuo
  (lambda (I)
    (/ (suma-cuadrados-distancia I 0 (promedio-individuo I)) (length I))))

;------ Desviación estándar de individuo ------
(define desviacion-estandar
  (lambda (I)
    (sqrt (varianza-individuo I))))

;----- Crear poblacion -------
(define poblacion-inicial
  (lambda (tamano acum)
    (if (zero? tamano) acum
        (poblacion-inicial (- tamano 1) (append acum (list (distribuir L _k)))))))

;----- Obtener desviaciones de poblacion -------
(define get-desviaciones
  (lambda (poblacion)
    (map desviacion-estandar poblacion)))
;---- Hash-map (key-value) de Individuo-desviacion----;
(define get-key-value
  (lambda (poblacion desviaciones pos)
    (list (list-ref poblacion pos) (list-ref desviaciones pos))))

(define genera-hash-map
  (lambda (poblacion acum) 
    (map (lambda (I)
                (append acum (list I (desviacion-estandar I)))) poblacion)))
;----- Ordena hash-map por desviacion ----;
(define sort-hash-map
  (lambda (hash-map)
    (sort hash-map #:key cadr <)))

;----- Obtiene primeros n individuos de hashmap ordenado -----
(define get-primeros-n
  (lambda (n iter acum sorted-hash-map)
    (if (= (- n 1) iter) acum
        (get-primeros-n n (+ iter 1) (append acum (list (car (list-ref sorted-hash-map iter)))) sorted-hash-map))))
 
(define get-first-n-individuos
  (lambda (sorted-hash-map n)
    (get-primeros-n n 0 '() sorted-hash-map)))

;----- Combinar dos individuos -----
(define combinar-individuos
  (lambda (padre1 padre2 pivote)
    (append (list-tail (reverse padre2) (- (length padre1) pivote)) (list-tail padre1 pivote))))

;------ Combina dos individuos random de poblacion----;
(define aux-combinar-random
  (lambda (poblacion I checksum)
    (if (null? I)
        (aux-combinar-random poblacion (combinar-individuos
                                        (list-ref poblacion (random (length poblacion)))
                                        (list-ref poblacion (random (length poblacion)))
                                        (random _k)) checksum)
        (if (= (suma-individuo I 0) checksum) I (aux-combinar-random poblacion null checksum)))))

(define individuo-existe?
  (lambda (poblacion I)
    (cond [(empty? poblacion) #f]
          [(equal? (car poblacion) I) #t]
          [else (individuo-existe? (cdr poblacion) I)])))

(define combinar-2-random
  (lambda (poblacion checksum)
    (aux-combinar-random poblacion null checksum)))

;----- Genera la cantidad de combinados a partir de una elite ----
(define generar-combinados
  (lambda (elite cuantos iter acum)
    (cond [(= iter cuantos) acum]
          [else
           (let* ([I (combinar-2-random elite (suma-individuo (car elite) 0))])
             (cond [(individuo-existe? elite I) (generar-combinados elite cuantos iter acum)]
                   [(individuo-existe? acum I) (generar-combinados elite cuantos iter acum)]
                   [else (generar-combinados elite cuantos (+ iter 1) (append acum (list I)))
                         ]))
           ])))
    

;============== MUTACION ==================
(define verificarListaVacia (lambda (L) (map (lambda (x) (if (empty? x) #t #f)) L)))

(define retornaSiVacia (lambda(L) (if(list? (member #t L))(first(member #t L)) #f)))

(define mutarPapi (lambda (L k m) (cond ((empty? m) (mutarPapi L k (mutacion L k))) ((equal?(retornaSiVacia (verificarListaVacia m))#t)(mutarPapi L k (mutacion L k))) (else m))))

(define mutacion (lambda (L k) (swapElem L (random k)(random k) '() k )))

(define swapEle (lambda ( random i j L R) (cond ((empty? R) (swapEle random i j (getMutedL L random i j '()) (getMutedR L random i j '()) ))          (else (append L R)))))

(define swapElem (lambda (L i j R k) (cond ((= i j) (swapElem L i (random k) '() k)) ((empty? R) (swapElem L i j (swapEle (random (length (list-ref L i))) i j L R) k)) (else R))))

(define getMutedR (lambda (L random i j R) (cond ((empty? R)( getMutedR L random i j (list (cons (list-ref (list-ref L i) random) (list-ref L j))))) (else  R))))

(define getMutedL
  (lambda (L random i j R)
  (cond ((empty? R)( getMutedL L random i j (append (list (remove (list-ref (list-ref L i) random) (list-ref L i)))  (remove (list-ref L j) (remove (list-ref L i) L ) ))))
        (else  R  ))))

;----- Genera la cantidad de mutados ----
(define generar-mutados
  (lambda (elite cuantos iter acum)
    (cond [(= iter cuantos) acum]
          [else
           (let* ([I (mutarPapi (list-ref elite (random (length elite))) _k '())])
             (cond [(individuo-existe? elite I) (generar-mutados elite cuantos iter acum)]
                   [(individuo-existe? acum I) (generar-mutados elite cuantos iter acum)]
                   [else (generar-mutados elite cuantos (+ iter 1) (append acum (list I)))]))
           ])))
;------- OBTENER NUEVA GENERACION ---------------
(define generar-nueva-poblacion
  (lambda (poblacion)
    (let* ([hash-poblacion (genera-hash-map poblacion '())]
       [ordenados (sort-hash-map hash-poblacion)]
       [elite (get-first-n-individuos ordenados (floor (/ (length poblacion) 2)))]
       [tam_elite (length elite)]
       [tam_combinados (floor (/ (- (length poblacion) tam_elite) 2))]
       [tam_mutados (- (length poblacion) (+ tam_elite tam_combinados))]
       [combinados (generar-combinados elite tam_combinados 0 '())]
       [mutados (generar-mutados elite tam_mutados 0 '())])
       (append elite combinados mutados))))
;=========================================
;[(= num_gen total) (display "               Mejor individuo                |     Desviacion std")(newline)
;                   (car (sort-hash-map (genera-hash-map poblacion '())))
;                ]
;------- Generaciones loop ---------------
(define loop-generaciones
  (lambda (total num_gen poblacion tam_poblacion)
    (cond [(or (empty? poblacion) (= num_gen 0)) (loop-generaciones total (+ num_gen 1) (poblacion-inicial tam_poblacion '()) tam_poblacion)]
          [(= num_gen total) poblacion];(sort-hash-map (genera-hash-map poblacion '()))]
          [else
           (display num_gen)
            (newline)
            (loop-generaciones total (+ num_gen 1) (generar-nueva-poblacion poblacion) tam_poblacion)])))
;-----------------------------------------
(loop-generaciones 3 0 null 30)

#| PRUEBAS DE CADA METODO
(define poblacion (poblacion-inicial _poblacion '()))
(define hash-poblacion (genera-hash-map poblacion '()))
(define ordenados (sort-hash-map hash-poblacion))
(define elite (get-first-n-individuos ordenados (floor (/ (length poblacion) 2))))
(define tam_elite (length elite))
(define tam_combinados (floor (/ (- (length poblacion) tam_elite) 2)))
(define tam_mutados (- (length poblacion) (+ tam_elite tam_combinados)))
(define combinados (generar-combinados elite tam_combinados 0 '()))
(define mutados (generar-mutados elite tam_mutados 0 '()))

(display "================ ORDENADOS ==================")(newline)
(sort-hash-map hash-poblacion)
(display "================ ELITE ==================")(newline)
(get-first-n-individuos ordenados (floor (/ (length poblacion) 2)))
(display "================ Tamano de Elite ==================")(newline)
(display tam_elite)(newline)
(display "================ Tamano de combinados ==================")(newline)
(display tam_combinados)(newline)
(display "================ Tamano de mutados ==================")(newline)
(display tam_mutados)(newline)
(display "================ COMBINADOS ==================")(newline)
(generar-combinados elite tam_combinados 0 '())
(display "================ MUTADOS ==================")(newline)
(generar-mutados elite tam_mutados 0 '())
|#
