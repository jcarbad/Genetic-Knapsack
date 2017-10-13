#lang racket


(define lst '(30 60 90 25 20 15 16 120 200 43 18 30 30))

(define tamano (length lst))

; Calcula todas las combinaciones de una lista
(define (conjuntoPotencia set)
	(if (null? set) '(())
		(let ((cP-resto (conjuntoPotencia (cdr set))))
			(append cP-resto
			(map (lambda (subset) (cons (car set) subset)) cP-resto)))))

;(conjuntoPotencia lst)

(define (cardinalidadCP set)
	(expt 2 (length set)))

(cardinalidadCP lst)

; Implementación sencilla del Knapsack
(define (knapsack lst)
  (define half (/ (apply + lst) 2))   ; compute half of total
  (printf "list  : ~a\nhalf  : ~a\n" lst half)
  (define (combs lst1 (lst2 null))    ; compute all the combinations  
    (if (null? lst1)
        (if (null? lst2) null
            (list (reverse lst2)))
        (append
         (combs (cdr lst1) lst2)      ; case 1 -> we don't carry the iten
         (combs (cdr lst1) (cons (car lst1) lst2)))))   ; case 2 -> we do
  (for/fold ((delta half) (res null)) ((c (in-list (combs lst)))) ; determine the best fit
    (let* ((sm (apply + c)) (newdelta (abs (- half sm))))
      (cond
        ((< newdelta delta) (values newdelta (list c)))
        ((= newdelta delta) (values    delta (cons c res)))
        (else               (values    delta res))))))
(time 
	(let-values (((delta res) (knapsack (cdr (range 0 24 3)))))
		(printf "result: ~a\ndelta : ~a\n" res delta)))