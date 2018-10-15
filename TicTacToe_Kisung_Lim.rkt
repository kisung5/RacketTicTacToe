#lang racket/Gui

;Kisung Lim
;Juego de tic tac toe o gato

(define this_width 600)
(define this_height 600)
(define sqrSize 0)
(define pX 0)
(define pY 0)

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
             (displayln (list pX pY))
             (drawEle pX pY (send this get-dc) 0))))
    ; Call the superclass init, passing on all init args
    (super-new)))

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

(define (drawX x y dc)
  (send dc set-pen red-pen)
  (send dc draw-line x y (-(+ x sqrSize)10) (-(+ y sqrSize)10))
  (send dc draw-line x (-(+ y sqrSize)10) (-(+ x sqrSize)10) y))

(define (drawO x y dc)
  (send dc set-pen blue-pen)
  (send dc draw-ellipse x y (-(+ x sqrSize)10) (-(+ y sqrSize)10)))

(define (drawEle pX pY dc type)
  (cond ((equal? type 0)
         ;(cond (first
                ;(send dc scale (/(*(/ sqrSize 600)(send xSign get-width))(send xSign get-width))
                           ;(/(*(/ sqrSize 600)(send xSign get-height))(send xSign get-width)))
                ;(set! first #f)))
         ;(displayln(call-with-values (thunk (send dc get-scale)) list))
         ;(send dc scale (/ sqrSize 600)(/ sqrSize 600))
         ;(flomap->bitmap (flomap-resize xSign (- sqrSize 10) (- sqrSize 10)))
         (drawX (+ (* sqrSize pX) 5)(+ (* sqrSize pY) 5)dc))
        (else (let()
               ;((draw-pixmap ventana) "x.png" (make-posn (+ (* 60 pX) 5) (+ (* 60 pY) 5)))
                ;(jugador ventana fila columna)
                #f))))

; Make a canvas that handles events in the frame
;(define gameCanvas(new gameCanvas% [parent frame]))
;(new gameCanvas% [parent frame])

(define (TTT M N)
  (cond ((or (or (< M 3) (> M 10)) (or (< N 3) (> N 10)))
         "Se requiere de tamaño mínimo de 3 y máximo de 10")
        (else( let()
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