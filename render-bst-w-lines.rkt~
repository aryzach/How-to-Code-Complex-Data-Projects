;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname render-bst-w-lines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




;; BST -> Image

(define (composite t)
  (overlay
   (lines t)
   (content t)))


;; BST -> Image
;; render the lines, use helper to have child node right length to determine length of left line from parent
(define (lines t) img)

;; BST String -> Integer
;; helper to find lenght of line, given BST and "right" or "left" to see what side it nees to go down, can also determine HSPACE
(define (lengthtree t "right") 0)

;; BST-> Image
;; content is a modified version from the video
(define (content t)
  (cond 	[(false? t) (square 0 "solid" "white")]
                [else
                 (above (text (string-append (number->string (node-key t)) ":" (node-val t)) 24 "black")
                        VSPACE
                        (beside (content (node-l t))
                                HSPACE
                                (content (node-r t))))]))