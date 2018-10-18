;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Greedy_Algorithm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(require racket)



; Verifica quedan movimientos por hacer

(define (isMovesLeft matrix M N)
  (isMovesLeftAux matrix M N 0 0))


(define (isMovesLeftAux matrix M N I J)
  (cond ((equal? I M)#f)
        ((equal? J N) (isMovesLeftAux matrix M N (+ I 1) 0))
        ((equal? (list-ref (list-ref matrix I) J) 0)#t)
        (else (isMovesLeftAux matrix M N I (+ J 1)))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Evaluacion de wins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Evalua si alguna linea ya esta completa
(define (evaluate matrix M N)
  (cond ((equal? (checkVertical matrix M N 0 0) 10) 10)
        ((equal? (checkVertical matrix M N 0 0) -10) -10)
        ((equal? (checkHorizontal matrix M N 0 0) 10) 10)
        ((equal? (checkHorizontal matrix M N 0 0) -10) -10)
        ;((equal? (checkDiagonal matrix M N 0 0) -10) -10)
        ;((equal? (checkDiagonal matrix M N 0 0) -10) -10)
        (else 0)))

  


; checkea si algun jugador gano por linea vertical
(define (checkVertical matrix M N I J)
  (cond ((equal? J N)0)
        ((equal? (+ I 1) M) (checkVertical matrix M N 0 (+ J 1)))
        ((and (not(zero? (list-ref (list-ref matrix I) J))) (equal? (list-ref (list-ref matrix I) J) (list-ref (list-ref matrix (+ I 1)) J)))
         (cond ((equal? (+ I 2) M)
                (cond ((equal? (list-ref (list-ref matrix I) J) 1) -10) 
                      (else 10)))
               (else (checkVertical matrix M N (+ I 1) J))))
        ((or (zero? (list-ref (list-ref matrix I) J))(not(equal? (list-ref (list-ref matrix I) J) (list-ref (list-ref matrix (+ I 1)) J))))
         (checkVertical matrix M N 0 (+ J 1)))
        ;(else 0)
        ))  



; checkea si algun jugador gano por linea horizontal
(define (checkHorizontal matrix M N I J)
  (cond ((equal? I N)0)
        ((equal? (+ J 1) N) (checkHorizontal matrix M N (+ I 1) 0))
        ((and (not(zero? (list-ref (list-ref matrix I) J))) (equal? (list-ref (list-ref matrix I) J) (list-ref (list-ref matrix I) (+ J 1))))
         (cond ((equal? (+ J 2) N)
                (cond ((equal? (list-ref (list-ref matrix I) J) 1) -10)
                      (else 10)))
               (else (checkHorizontal matrix M N I (+ J 1)))))
        ((or (zero? (list-ref (list-ref matrix I) J))(not(equal? (list-ref (list-ref matrix I) J) (list-ref (list-ref matrix I) (+ J 1)))))
         (checkHorizontal matrix M N (+ I 1) 0))
        ;(else 0)
        )) 
 



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


(define (findBestMoveAux matrix bestValue M N I J  X Y) 
  (cond ((equal? I M) (list X (- Y 1)));;;;; OJO AQUI HAY QUE CAMBIAR
        ((equal? J N) (findBestMoveAux matrix bestValue M N (+ I 1) 0 X Y))
        ((zero? (list-ref (list-ref matrix I) J))
         (minimaxValue (setMatrixValue matrix I J 0) bestValue (minimax (setMatrixValue matrix I J -1) 0 false M N 0 0 (evaluate matrix M N) ) M N I (+ J 1) X Y))
        (else (findBestMoveAux matrix bestValue M N I (+ J 1) X Y))))





(define (minimaxValue matrix bestValue moveValue M N I J X Y)
  (findBestMoveAux matrix moveValue M N I J I J))




; funcion de miniMax, encuentra el mejor movimiento
; minimax(lista matrix, int depth, bool isMax, int M, int N, int I, int J, int score)

(define (minimax matrix depth isMax M N I J score)
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
  

(define (setMatrixValue matrix I J value)
  (list-set matrix I (list-set (list-ref matrix I) J value)))

(provide findBestMove)
 