;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Greedy_Algorithm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket)

;Devuelve el número más alto de la lista.
(define (getMax lista num)
  (cond ((null? lista)
         num)
        ((>= (car lista) num)
         (getMax (cdr lista) (car lista)))
        (else
         (getMax (cdr lista) num))))

;Devuelve el número más bajo de una lista.
(define (getMin lista num)
    (cond ((null? lista)
         num)
        ((<= (car lista) num)
         (getMin (cdr lista) (car lista)))
        (else
         (getMin (cdr lista) num))))

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
        ((checkDiagonal matrix M N 1) 10)
        ((checkDiagonal matrix M N -1) -10)
        (else 0)))

; checkea si algun jugador gano por linea vertical.
(define (checkVertical matrix M N I J num)
  (cond ((equal? J N)#f)
        ((equal? I M)#f)
        ((and(equal? (getMatrix matrix I J) num)(checkVerticalAux matrix M N (+ I 1) J num))
         #t)
        (else (checkVertical matrix M N 0 (+ J 1) num))))

(define (checkVerticalAux matrix M N I J num)
  (cond ((equal? J N)#f)
        ((equal? I M)#t)
        ((and(equal? (getMatrix matrix I J) num)(checkVerticalAux matrix M N (+ I 1) J num))
         #t)
        (else #f)))
  
; checkea si algun jugador gano por linea horizontal.
(define (checkHorizontal matrix M N I J num)
  (cond ((equal? I M)#f)
        ((equal? J N)#f)
        ((and(equal?(getMatrix matrix I J) num)(checkHorizontalAux matrix M N I (+ J 1) num))
         #t)
        (else (checkHorizontal matrix M N (+ I 1) J num)))) 

(define (checkHorizontalAux matrix M N I J num)
  (cond ((equal? I M)#f)
        ((equal? J N)#t)
        ((and(equal?(getMatrix matrix I J) num)(checkHorizontalAux matrix M N I (+ J 1) num))
         #t)
        (else #f)))  

; Checkea si algun jugador gano por diagonal. 
(define (checkDiagonal matrix M N num)
  (cond ((checkFirstDiagonal  matrix M N 0 0 num) #t)
        ((checkSecondDiagonal  matrix M N 0 0 num) #t)
        ;((checkThirdDiagonal  matrix M N 0 N num) #t)
       ; ((checkFourthDiagonal  matrix M N 0 N num) #t)
        (else #f)))
         


; Diagonales de Izquierda a derecha
(define (checkFirstDiagonal matrix M N I J num)
  (cond ((equal? J N) #f)
        ((or (>= (+ I 2) M) (>= (+ J 2) N))
         (checkFirstDiagonal matrix M N 0 (+ J 1) num))
        ((and (equal? (getMatrix matrix I J) (getMatrix matrix (+ I 1) (+ J 1)))
              (equal? (getMatrix matrix (+ I 1) (+ J 1)) (getMatrix matrix (+ I 2) (+ J 2)))
              (equal? (getMatrix matrix (+ I 2) (+ J 2)) num))
         (cond ((or (>= (+ I 3) M) (>= (+ J 3) N))
                #t)
               (else (checkFirstDiagonal matrix M N (+ I 1) (+ J 1) num))))
        (else (checkFirstDiagonal matrix M N 0 (+ J 1) num))))
         





; Diagonales de arriba a abajo, lado izquierdo

(define (checkSecondDiagonal matrix M N I J num)
  (cond ((equal? I M) #f)
        ((or (>= (+ I 2) M) (>= (+ J 2) N))
         (checkSecondDiagonal matrix M N (+ I 1) 0 num))
        ((and (equal? (getMatrix matrix I J) (getMatrix matrix (+ I 1) (+ J 1)))
              (equal? (getMatrix matrix (+ I 1) (+ J 1)) (getMatrix matrix (+ I 2) (+ J 2)))
              (equal? (getMatrix matrix (+ I 2) (+ J 2)) num))
         (cond ((or (>= (+ I 3) M) (>= (+ J 3) N))
                #t)
               (else (checkSecondDiagonal matrix M N (+ I 1) (+ J 1) num))))
        (else (checkSecondDiagonal matrix M N (+ I 1) 0 num))))



;diagonales de derecha a izquierda
(define (checkThirdDiagonal matrix M N I J num)
  (cond ((< N 0) #f)
        ((or (< (- J 2) 0) (>= (+ I 2) M))
         (checkThirdDiagonal matrix M (- N 1) 0 (- N 1) num))
        ((and (equal? (getMatrix matrix I J) (getMatrix matrix (+ I 1) (- J 1)))
              (equal? (getMatrix matrix (+ I 1) (- J 1)) (getMatrix matrix (+ I 2) (- J 2)))
              (equal? (getMatrix matrix (+ I 2) (- J 2)) num))
         (cond ((or (>= (+ I 3) M) (< (- J 3) 0))
                #t)
               (else (checkThirdDiagonal matrix M N (+ I 1) (- J 1) num))))
        (else (checkThirdDiagonal matrix  M (- N 1) 0 (- N 1) num))))



;diagonales de arriba hacia abajo, lado derecho
(define (checkFourthDiagonal matrix M N I J num)
  (cond ((equal? I M) #f)
        ((or (< (- J 2) 0) (>= (+ I 2) M))
         (checkFourthDiagonal matrix M N (+ I 1) N num))
        ((and (equal? (getMatrix matrix I J) (getMatrix matrix (+ I 1) (- J 1)))
              (equal? (getMatrix matrix (+ I 1) (- J 1)) (getMatrix matrix (+ I 2) (- J 2)))
              (equal? (getMatrix matrix (+ I 2) (- J 2)) num))
         (cond ((or (>= (+ I 3) M) (< (- J 3) 0))
                #t)
               (else (checkFourthDiagonal matrix M N (+ I 1) (- J 1) num))))
        (else (checkFourthDiagonal matrix  M N (+ I 1) N num))))
    
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
         (cond ((> (minimax (setMatrixValue matrix I J 1)
                          0 #f M N 0 0 (evaluate matrix M N)) bestValue)
                (findBestMoveAux matrix
                                 (minimax (setMatrixValue matrix I J 1)
                                          0 #f M N 0 0 (evaluate matrix M N))
                                 M N
                                 I (+ J 1)
                                 I J))
               (else (findBestMoveAux matrix bestValue M N I (+ J 1) X Y))))
         (else
          (findBestMoveAux matrix bestValue M N I (+ J 1) X Y))))

; funcion de miniMax, encuentra el mejor movimiento
; minimax(lista matrix, int depth, bool isMax, int M, int N, int I, int J, int score)

(define (minimax matrix depth isMax M N I J score)
  (cond ((equal? depth 3)
         score)
        ((equal? score 10)
         score)
        ((equal? score -10)
         score)
        ((equal? (isMovesLeft matrix M N) #f)
         0)
        (isMax
         (maximizer matrix -1000 M N 0 0 isMax depth))
        (else
         (minimizer matrix 1000 M N 0 0 isMax depth))))

(define (maximizer matrix best M N I J isMax depth)
  (cond ((equal? I M)
         best)
        ((equal? J N)
         (maximizer matrix best M N (+ I 1) 0 isMax depth))                                          
        ((equal?(getMatrix matrix I J) 0)
         (maximizer matrix
                    (max best (minimax (setMatrixValue matrix I J 1)
                             (+ depth 1) (not isMax) M N 0 0
                             (evaluate(setMatrixValue matrix I J 1) M N)))
                    M N I (+ J 1) isMax depth))
        (else
         (maximizer matrix best
                    M N I (+ J 1) isMax depth))))
        
(define (minimizer matrix best M N I J isMax depth) 
  (cond ((equal? I M)
         best)
        ((equal? J N)
         (minimizer matrix best M N (+ I 1) 0 isMax depth))
        ((equal?(getMatrix matrix I J) 0)
         (minimizer matrix
                    (min best(minimax (setMatrixValue matrix I J -1)
                             (+ depth 1) (not isMax) M N 0 0
                             (evaluate (setMatrixValue matrix I J -1) M N)))
                    M N I (+ J 1) isMax depth))
        (else
         (minimizer matrix best
                    M N I (+ J 1) isMax depth)))) 

(provide findBestMove
         candidateSet
         evaluate)