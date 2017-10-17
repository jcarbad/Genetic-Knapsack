#lang racket
; Ejemplo de algoritmo genetico
; https://ericscrivner.me/2015/07/implementing-genetic-algorithms-in-scheme/
; https://gist.github.com/etscrivner/f18ffa843d7eb33995c6

; Genera bit random (1,0 aleatorio)
(define (random-bit) (random 2))

; Genera lista de [num-bits] cantidad de bits 'el genoma'
(define (make-random-genome num-bits)
  (map (lambda (x) (random-bit)) (iota num-bits 1)))

; Invierte un cierto bit de un array de bits de [num-bits] tamaño
(define (make-probabilistic-bit-flipper num-bits)
  (define (bit-flipper bit)
    (let ((bit-array (append '(1) (make-list (- num-bits 1) 0))))
      (if (= 1 (list-ref bit-array (random num-bits)))
          (if (= bit 0) 1 0)
          bit)))
  bit-flipper)

; Aplica inversion a todos los bits del genoma (mutacion)
(define (mutate genome)
  (map (make-probabilistic-bit-flipper (length genome)) genome))

; Recombina dos genomas a partir de un punto aleatorio de las listas
(define (crossover g1 g2)
  (let ((crossover-point (random (length g1))))
    (cons (append (list-head g1 crossover-point)
                  (list-tail g2 crossover-point))
          (append (list-head g2 crossover-point)
                  (list-tail g1 crossover-point)))))

; Genera una poblacion de tamano [population-size]
(define (make-random-population population-size make-member-func)
  (map (lambda (x) (make-member-func)) (iota population-size 1)))

; Obtiene la 'fitness' de un individuo mediante una funcion de fitness
(define (get-member-fitness population fitness-func)
  (map fitness-func population))

; Obtiene la fitness promedio de una poblacion
(define (get-average-fitness population fitness-func)
  (/ (reduce + 0 (get-member-fitness population fitness-func))
     (exact->inexact (length population))))

; Mecanismo para obtener el genoma del fittest individuo
(define (random-shuffle lst)
  (sort lst (lambda (x y) (equal? 0 (random 2)))))
 
(define (make-random-bit-mask size num-ones)
  (if (< size num-ones)
      (error "Invalid size -- MAKE-RANDOM-BIT-MASK" size num-ones)
      (random-shuffle (append (make-list num-ones 1)
                              (make-list (- size num-ones) 0)))))
 
(define (select-for-tournament population tournament-size)
  (map car
     (filter (lambda (x) (= (cdr x) 1))
             (map cons
                population
                (make-random-bit-mask (length population)
                                      tournament-size)))))
 
 
(define (select-fittest population fitness-func num-to-select)
  (list-head
   (sort population
         (lambda (x y) (> (fitness-func x) (fitness-func y))))
   num-to-select))

; Seleccion de los n miembros mas fit
(define (next-generation population fitness-func)
  (define (produce-next-generation-member)
    (if (= 1 (random 2))
        (let ((tournament-members (select-for-tournament population 3)))
          ((if (= 1 (random 2)) car cdr)
           (apply crossover
                  (select-fittest tournament-members fitness-func 2))))
        (mutate (car (select-for-tournament population 1)))))
  (map (lambda (x) (produce-next-generation-member))
     (iota (length population) 1)))

; Optimizacion de algo
(define (bits->int genome)
  (reduce + 0
   (map (lambda (x y) (* x (expt 2 y)))
      genome
      (reverse (map (lambda (x) (- x 1)) (iota (length genome) 1))))))
 
(define population
  (make-random-population 10 (lambda () (make-random-genome 10))))
 
(next-generation population bits->int)