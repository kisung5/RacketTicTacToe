;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToe_Kisung_Pablo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/gui)
(require "Greedy_Kisung_Pablo.rkt")

;Kisung Lim
;Pablo Esquivel Morales
;Juego de tic tac toe o gato

(define this_width 600)
(define this_height 600)
(define sqrSize 0)
(define pX 0)
(define pY 0)
(define Mp 0)
(define Np 0)

(define gameTable '())

(define red-pen (make-object pen% "RED" 4 'solid))
(define blue-pen (make-object pen% "BLUE" 4 'solid))

(define xSign(make-object bitmap% "x.png"))

(define (calcSqr max)
  (set! sqrSize (/ 600 max)))

(define (aproxPos x)
  (floor ( / x sqrSize)))

; Derive a new canvas (a drawing window) class to handle events
(define gameCanvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (cond ((send event button-down? 'left)
             (set! pX(aproxPos(send event get-x)))
             (set! pY(aproxPos(send event get-y)))
             (cond ((or(hasAlredy pY pX gameTable 1)(hasAlredy pY pX gameTable -1))
                    (displayln "Repeticion"))
                   (else
                    (displayln (list pX pY))
                    (set! gameTable(putM pY pX gameTable 1))
                    (displayln gameTable)
                    (drawEle pX pY (send this get-dc) 0)
                   ;Aqui correria el algoritmo codicioso.
                    (displayln(candidateSet gameTable 0 0 Np Mp 0))
                    )))))
    ; Call the superclass init, passing on all init args
    (super-new)))

;Dibujado del tablero.
(define (drawColumn fila columna lineCount columnCount dc)
  (cond((equal? columna columnCount))
       (else (let()
               (drawLine fila columna lineCount columnCount dc)
               (send dc draw-rectangle(* lineCount sqrSize)(* columnCount sqrSize)sqrSize sqrSize)
               (drawColumn fila columna lineCount (+ 1 columnCount) dc)))))

(define (drawLine fila columna lineCount columnCount dc)
  (cond((equal? fila lineCount))
       (else (let()
              (send dc draw-rectangle(* lineCount sqrSize)(* columnCount sqrSize)sqrSize sqrSize)
              (drawLine fila columna (+ 1 lineCount) columnCount dc)))))

;Definiciones para dibujado.
(define (drawX x y dc)
  (send dc set-pen red-pen)
  (send dc draw-line x y (-(+ x sqrSize)10) (-(+ y sqrSize)10))
  (send dc draw-line x (-(+ y sqrSize)10) (-(+ x sqrSize)10) y))

(define (drawO x y dc)
  (send dc set-pen blue-pen)
  (send dc draw-ellipse x y (-(+ x sqrSize)10) (-(+ y sqrSize)10)))

(define (drawEle pX pY dc type)
  (cond ((equal? type 0)
         (drawX (+ (* sqrSize pX) 5)(+ (* sqrSize pY) 5)dc))
        ((equal? type 1)
         (drawO (+ (* sqrSize pX) 5)(+ (* sqrSize pY) 5)dc))))

;Crea la matriz inicial para el tablero en Gui.
(define (makeTable x y M N matrix)
  (cond ((equal? x N)
         matrix)
        (else (append (list (makeTableAux x y M N matrix))(makeTable (+ x 1)y M N matrix)))))

(define (makeTableAux x y M N matrix)
  (cond ((equal? y M)
         matrix)
        (else (append (list 0) (makeTableAux x (+ y 1) M N matrix)))))

;Pone la ficha en el trablero.
(define (putM x y matrix num)
  (cond ((equal? x 0)
         (append (list(putM_aux y (car matrix) num))(cdr matrix)))
        (else (append (list(car matrix))(putM (- x 1) y (cdr matrix) num)))))

(define (putM_aux y matrix num)
  (cond ((equal? y 0)
         (cons num (cdr matrix)))
        (else (cons (car matrix)(putM_aux (- y 1) (cdr matrix) num)))))

;Verifica la existencia del elemento en el tablero.
(define (hasAlredy x y matrix num)
  (cond ((equal? x 0)
         (hasAlredy_aux y (car matrix) num))
        (else (hasAlredy (- x 1) y (cdr matrix) num))))

(define (hasAlredy_aux y matrix num)
  (cond ((equal? y 0)
         (equal? num (car matrix)))
        (else (hasAlredy_aux (- y 1) (cdr matrix) num))))

; Make a canvas that handles events in the frame
(define (TTT M N)
  (cond ((or (or (< M 3) (> M 10)) (or (< N 3) (> N 10)))
         "Se requiere de tamaño mínimo de 3 y máximo de 10")
        (else( let()
                (set! Mp M)
                (set! Np N)
                (set! gameTable (makeTable 0 0 M N '())) 
                (define frame(new frame%
                                  [label "Tic Tac Toe"]
                                  [stretchable-height #f]
                                  [stretchable-width #f]))
                (cond ((> M N)(calcSqr M))
                      (else(calcSqr N)))
                (send frame min-width(* sqrSize M))
                (send frame min-height(* sqrSize N))
                (define gameCanvas(new gameCanvas% [parent frame]
                                       [paint-callback
                                        (lambda (canvas dc)
                                          (drawColumn M N 0 0 dc))]))
                (send frame show #t))))) 