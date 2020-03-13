;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname htc3bproject) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a program that when you click, an image appears and grows until you click again, then a new image appears and grows

(require 2htdp/image)
(require 2htdp/universe)
;; Constants
 
(define WIDTH 500)
(define HEIGHT 500)
 
(define MTS (empty-scene WIDTH HEIGHT))
 
(define rate 1)
 
;; Data definitions
 
(define-struct growpic (x y w h onoff))
;; growpic is a (make-growpic Natural[0, WIDTH] Natural[0, HEIGHT] Natural[0, WIDTH] Natural[0, HEIGHT] One of: On Off)
;; interp. growpic as a shape with an x and y position, and dimensions of w and h, on/off is whether to put shape or not
(define G1 (make-growpic 5 6 7 8 "off"))
(define (fn-for-grow-pic g)
                ( ...          (growcpic-x g)
                                (growcpic-y g)
                                (growcpic-w g)
                                (growcpic-h g)
                                (growpic-onoff g)))
; Template rules used:
;               Compound: 2 fields, 2 of each, 1 enumeration
;                               - atomic non-distinct:
;                                                               - Natural[0, WIDTH]
;                                                               - Natural[0, HEIGHT]
;                               - atomic distinct: One of: "on", "off"
 
;; Functions
;; growpic -> growpic
;; start the world with (main (make-growpic 0 0 0 0 "off"))
(define (main g)
  (big-bang g                  ; growpic
    (on-tick update)           ; growpic -> growpic
    (to-draw render)           ; growpic -> image
    (on-mouse new)))           ; growpic integer integer MouseEvent -> growpic
 
 
;; growpic -> growpic
;; make the shape bigger on each tick
(check-expect (update (make-growpic 6 7 1 2 "off")) (make-growpic 6 7 2 3 "off"))
(check-expect (update (make-growpic 6 7 70 30 "on")) (make-growpic 6 7 71 31 "on"))
;(define (update g) g) ;stub
; <Temp from growpic>
(define (update g)
                (make-growpic (growpic-x g) (growpic-y g) (+ (growpic-w g) rate) (+ (growpic-h g) rate) (growpic-onoff g)))
 
;; growpic -> image
;; put the new or updated shape on the display
(check-expect (render (make-growpic 5 6 7 8 "on")) (place-image (rectangle 7 8 "solid" "orange") 5 6 MTS))
(check-expect (render (make-growpic 5 6 7 8 "off")) (place-image (rectangle 1 1 "solid" "white") 0 0 MTS))
;(define (render g) MTS) ;stub
; <Temp from growpic>

(define (render g)
  (cond [(string=? (growpic-onoff g) "off")
         (place-image (rectangle 1 1 "solid" "white") 0 0 MTS)]
        [else (place-image (get-shape g) (growpic-x g) (growpic-y g) MTS)]))
#;
(define (render g)
  (place-image (get-shape g) (growpic-x g) (growpic-y g) MTS))

;; growpic integer integer MouseEvent -> growpic
;; make new growpic in new location
(check-expect (new (make-growpic 0 0 0 0 "off") 5 6 "button-up") (make-growpic 0 0 0 0 "off"))
(check-expect (new (make-growpic 0 0 0 0 "off") 5 6 "button-down") (make-growpic 5 6 5 10 "on"))
(check-expect (new (make-growpic 1 2 3 4 "on") 35 26 "button-down") (make-growpic 35 26 5 10 "on"))
;(define (new g) g) ;stub
; <Temp from growpic>
(define (new g i1 i2 me)
  (cond [(string=? me "button-down")
                ( make-growpic i1 i2 5 10 "on" )]
        [else g]))
 
;; growpic -> image
;; make the approriate sized shape to give to render
(check-expect (get-shape (make-growpic 1 2 3 4 "on")) (rectangle 3 4 "solid" "orange"))
;(define (get-shape g) (rectangle 1 2 "solid" "orange")) ;stub
; <Temp from growpic>
(define (get-shape g)
                ( rectangle (growpic-w g) (growpic-h g) "solid" "orange"))
 
 
 
 
 
 
 

 
