;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ttt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))

 
;; Board -> (listof Board)
;; create a list of all possible ending boards from the given board. No rules apply (ie could be all X's)
(check-expect (ttt B2) (list (list "X"  "X"  "O"     
                                   "O"  "X"  "O"
                                   "X"  "X" "X")
                             (list "X"  "X"  "O"  
                                   "O"  "X"  "O"
                                   "X"  "O" "X")))
(check-expect (ttt (list "X"  "X"  "O"     
                         "O"  "X"  "O"
                         "X" false false))
              (list (list "X"  "X"  "O"     
                          "O"  "X"  "O"
                          "X"  "X" "X")
                    (list "X"  "X"  "O"  
                          "O"  "X"  "O"
                          "X"  "O" "X")
                    (list "X"  "X"  "O"     
                          "O"  "X"  "O"
                          "X"  "O" "O")
                    (list "X"  "X"  "O"  
                          "O"  "X"  "O"
                          "X"  "X" "O")))
                             
(define (ttt bd)
  (local [(define (ttt--bd bd)
            (cond     [(full? bd) bd]
                      [else
                       (ttt--lobd (next-boards bd))]))
          (define (ttt--lobd lobd)
            (cond    [(empty? lobd) empty]
                     [else
                      (cons     (ttt--bd (first lobd))
                                  (ttt--lobd (rest lobd)))]))]
    (ttt--bd bd)))
 
 
;; Board -> Boolean
;; produce true if the board is full
;; !!!
;(define (full? bd) false) ;stub
(define (full? bd)
  (cond    [(empty? bd) true]
           [else
            (if            (false? (first bd))
                           false
                           (full? (rest bd)))]))
 
 
;; Board -> (listof Board)
;; create a list of the 2 sub boards from the current board
;; Assume: Board is not full
;; !!!
;(define (next-boards bd) empty) ;stub
(define (next-boards bd)
  (fill-with-X-O (first-false-index bd) bd))
 
;; Board -> Pos (define pos as Natural[0,8])
;; get pos of first false
;(define (first-false-index bd) 0) ;stub
(define (first-false-index bd)
  (cond    [(empty? bd) (error "should not be full board")]
           [else
            (if            (false? (first bd))
                           0
                           (+ 1 (first-false-index (rest bd))))]))
 
;; Pos Board -> (listof Board)
;; create a list of two board with the first empty position filled in with X and O
           
;(define (fill-with-X-O p bd) empty) ;stub

(define (fill-with-X-O p bd)
  (local [(define (fill-it-with n)
            (fill-square bd p n))]
    (map fill-it-with (list "X" "O"))))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))
 
;; part 3
 
 
;; (listof Board) -> (listof Board)
;; filter so only those remain with 5 X and 4 O
(define (filter-boards lobd)
  (filter valid-board? lobd))
 
;; Board -> Boolean
;; true if board has 5 X and 4 O
;; Assume: Board is full
(define (valid-board? bd)
  (= 5 (count-Xs (filter only-X bd))))
 
;; Value -> Boolean
(define (only-X v)
  (cond     [(false? v) (error "board not full")]
            [(string=? v "X") true]
            [(string=? v "O") false]))
 
;; (listof X) -> Boolean
;; true if there exist 5 Xs in the list of only Xs
(define (count-Xs lox)
  (if            (empty? lox)
                 0
                 (+ 1 (count-Xs (rest lox)))))
               
