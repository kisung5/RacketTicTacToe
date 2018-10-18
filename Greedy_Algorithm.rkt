;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Greedy_Algorithm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket)

;Devuelve el valor buscado de la posicion de la matriz.
(define (getMatrix matrix x y)
  (cond ((null? matrix)
         #f)
        ((equal? x 0)
         (getMatrixAux (car matrix) y))
        (else
         (getMatrix(cdr matrix)(- x 1)y))))

(define (getMatrixAux matrix y)
  (cond ((null? matrix)
         #f)
        ((equal? y 0)
         (car matrix))
        (else
         (getMatrixAux(cdr matrix)(- y 1)))))

;Devuelve una matriz con el valor agregado en la posicion. 
(define (setMatrixValue matrix x y value)
  (cond ((null? matrix)
         '())
        ((equal? x 0)
         (append (list(setMatrixValueAux (car matrix) y value))(cdr matrix)))
        (else
         (append (list(car matrix))(setMatrixValue(cdr matrix)(- x 1) y value)))))

(define (setMatrixValueAux matrix y value)
  (cond ((null? matrix)
         '())
        ((equal? y 0)
         (cons value (cdr matrix)))
        (else
         (append(list(car matrix))(setMatrixValueAux(cdr matrix)(- y 1) value)))))

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

; Verifica quedan movimientos por hacer
(define (isMovesLeft matrix M N)
  (cond ((equal? (candidateSet matrix 0 0 M N 0) '())
         #f)
        (else #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Evaluacion de wins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M filas N columnas
; Evalua si alguna linea ya esta completa
(define (evaluate matrix M N)
  (cond ((checkVertical matrix M N 0 0 1)
         10)
        ((checkVertical matrix M N 0 0 -1)
         -10)
        ((checkHorizontal matrix M N 0 0 1)
         10)
        ((checkHorizontal matrix M N 0 0 -1)
         -10)
        ;((equal? (checkDiagonal matrix M N 0 0) -10) -10)
        ;((equal? (checkDiagonal matrix M N 0 0) -10) -10)
        (else 0)))

; checkea si algun jugador gano por linea vertical.
(define (checkVertical matrix M N I J num)
  (cond ((equal? J N)#f)
        ((equal? I M)#t)
        ((and(equal? (getMatrix matrix I J) num)(checkVertical matrix M N (+ I 1) J num))
         #t)
        (else (checkVertical matrix M N 0 (+ J 1) num))))

; checkea si algun jugador gano por linea horizontal.
(define (checkHorizontal matrix M N I J num)
  (cond ((equal? I M)#f)
        ((equal? J N)#t)
        ((and(equal?(getMatrix matrix I J) num)(checkHorizontal matrix M N I (+ J 1) num))
         #t)
        (else (checkHorizontal matrix M N (+ I 1) J num)))) 

; Checkea si algun jugador gano por diagonal. 
(define (checkDiagonal matrix M N)
  (1))

(define (DiagonalLeftToRight matrix M N I J)
  (cond ((equal? J N) #f)
        (else (DiagonalAux matrix M N 0 J J))))

(define (DiagonalAux matrix M N I J Jtemp)
  (cond ((equal? (list-ref (list-ref matrix I) Jtemp) 0)
         (DiagonalLeftToRight matrix M N 0 (+ J 1)))
        ((or (>= (+ I 2) M) (>= (+ Jtemp 2) N)) #t)
        (else (cond ((and (equal? (list-ref (list-ref matrix I) Jtemp) (list-ref (list-ref matrix (+ I 1)) (+ Jtemp 1))) (equal? (list-ref (list-ref matrix (+ I 1)) (+ Jtemp 1)) (list-ref (list-ref matrix (+ I 2)) (+ Jtemp 2))))
                     (DiagonalAux matrix M N (+ I 1) J (+ J 1)))
                    (else (DiagonalLeftToRight matrix M N 0 (+ J 1))))))) 
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (findBestMove matrix M N)
  (findBestMoveAux matrix -1000 M N 0 0 -1 -1))


(define (findBestMoveAux matrix bestValue M N I J X Y) 
  (cond ((equal? I M)
         (list X Y))
        ((equal? J N)
         (findBestMoveAux matrix bestValue M N (+ I 1) 0 X Y))
        ((equal? (getMatrix matrix I J) 0)
         (cond (((minimax (setMatrixValue matrix I J 1) 0 false M N 0 0 (evaluate matrix M N)) > bestValue)
                (findBestMoveAux matrix
                                 (minimax (setMatrixValue matrix I J 1) 0 false M N 0 0 (evaluate matrix M N))
                                 M N
                                 I (+ J 1)
                                 I J))
               (else (findBestMoveAux matrix bestValue M N I (+ J 1) X Y))))
        ))

;Original
;(define (findBestMoveAux matrix bestValue M N I J X Y) 
;  (cond ((equal? I M)
;         (list X Y))
;        ((equal? J N)
;         (findBestMoveAux matrix bestValue M N (+ I 1) 0 X Y))
;        ((equal? (getMatrix matrix I J) 0)
;         (cond ((minimax()))
;         (minimaxValue (setMatrixValue matrix I J 0) bestValue (minimax (setMatrixValue matrix I J -1) 0 false M N 0 0 (evaluate matrix M N) ) M N I (+ J 1) X Y))
;        (else (findBestMoveAux matrix bestValue M N I (+ J 1) X Y))))



(define (minimaxValue matrix bestValue moveValue M N I J X Y)
  (findBestMoveAux matrix moveValue M N I J I J))




; funcion de miniMax, encuentra el mejor movimiento
; minimax(lista matrix, int depth, bool isMax, int M, int N, int I, int J, int score)

(define (minimax matrix depth isMax M N I J score best)
  (cond ((equal? score 10) score)
        ((equal? score -10) score)
        ((equal? (isMovesLeft matrix M N) #f) 0)
        ((equal? isMax #t) (maximizer matrix -1000 M N 0 0 isMax depth))
        (else (minimizer matrix 1000 M N 0 0 isMax depth))))




(define (maximizer matrix best M N I J isMax depth)
  (cond ((equal? I M) best)
        ((equal? J N) (maximizer matrix best M N (+ I 1) 0 isMax depth))                                            ;(setMatrixValue matrix I J -1) 
        ((zero? (list-ref (list-ref matrix I) J)) (maximizer (setMatrixValue matrix I J 0)
                                                             (minimax (setMatrixValue matrix I J -1) (+ depth 1) (not isMax) M N 0 0 (evaluate (setMatrixValue matrix I J -1) M N))
                                                              M N I (+ J 1) isMax depth))
        (else (maximizer matrix best M N I (+ J 1) isMax depth))))
        

(define (minimizer matrix best M N I J isMax depth) 
  (cond ((equal? I M) best)
        ((equal? J N) (minimizer matrix best M N (+ I 1) 0 isMax depth))                                                              ;(list-set matrix (list-ref (list-ref matrix I) J) 1)
        ((zero? (list-ref (list-ref matrix I) J)) (minimizer (setMatrixValue matrix I J 0)
                                                             (minimax (setMatrixValue matrix I J 1) (+ depth 1) (not isMax) M N 0 0  (evaluate (setMatrixValue matrix I J 1) M N ))
                                                              M N I (+ J 1) isMax depth))
        (else (minimizer matrix best M N I (+ J 1) isMax depth)))) 

(provide findBestMove)
 