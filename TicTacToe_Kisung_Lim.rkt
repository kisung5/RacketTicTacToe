#lang racket/Gui

;Kisung Lim
;Juego de tic tac toe o gato
(require images/flomap)

(define this_width 600)
(define this_height 600)
(define sqrSize 0)
(define pX 0)
(define pY 0)
(define first #t)

(define xSign(make-object bitmap% "x.png"))

(define (calcSqr max)
  (set! sqrSize (/ 600 max)))

(define (aproxPos x)
  (floor ( / x sqrSize)))

(define get-Type
  (lambda (x)
    (cond ((number? x) "Number")
          ((pair? x) "Pair")
          ((string? x) "String")
          ((list? x) "List")))) 

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

(define (drawEle pX pY dc type)
  (cond ((equal? type 0)
         ;(cond (first
                ;(send dc scale (/(*(/ sqrSize 600)(send xSign get-width))(send xSign get-width))
                           ;(/(*(/ sqrSize 600)(send xSign get-height))(send xSign get-width)))
                ;(set! first #f)))
         ;(displayln(call-with-values (thunk (send dc get-scale)) list))
         ;(send dc scale (/ sqrSize 600)(/ sqrSize 600))
         ;(flomap->bitmap (flomap-resize xSign (- sqrSize 10) (- sqrSize 10)))
         (send dc draw-bitmap xSign (+ (* sqrSize pX) 5) (+ (* sqrSize pY) 5)))
        (else (let()
               ;((draw-pixmap ventana) "x.png" (make-posn (+ (* 60 pX) 5) (+ (* 60 pY) 5)))
                ;(jugador ventana fila columna)
                #f))))

; Make a canvas that handles events in the frame
;(define gameCanvas(new gameCanvas% [parent frame]))
;(new gameCanvas% [parent frame])

(define (TTT M N)
  (define frame(new frame%
                  [label "Tic Tac Toe"]
                  [stretchable-height #f]
                  [stretchable-width #f]))
  (cond ((> M N)(calcSqr M))
        (else(calcSqr N)))
  (send frame min-width(* sqrSize M))
  (send frame min-height(* sqrSize N))
  (set! first #t)
  (define gameCanvas(new gameCanvas% [parent frame]
                         [paint-callback
                          (lambda (canvas dc)
                            (drawColumn M N 0 0 dc))]))
  (send frame show #t)) 