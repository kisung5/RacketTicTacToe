;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Greedy_Kisung_Pablo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;Kisung Lim
;Pablo Esquivel
;Algoritmo codicioso.
(require racket)

;Define el conjunto de:
;Espacios vacios = 0 (candidatos).
;Espacios X = 1 (jugador).
;Espacios O = -1 (maquina).
(define (candidateSet matrix x y M N num)
  (cond ((equal? x M)
                 '())
        (else
         (append(candidateSetAux (car matrix) x y M N num)
                (candidateSet (cdr matrix) (+ x 1) y M N num)))))

(define (candidateSetAux matrix x y M N num)
  (cond ((equal? y N)
         '())
        (else
         (cond ((equal? (car matrix) num)
                (cons (list x y)(candidateSetAux (cdr matrix) x (+ y 1) M N num)))
               (else
                (candidateSetAux (cdr matrix) x (+ y 1) M N num))))))

(provide candidateSet)