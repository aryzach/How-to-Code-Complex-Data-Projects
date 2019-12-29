;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |sink final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; blob is one of:

;; - "bubble"

;; - "solid"



(define B "bubble")

(define S "solid")



(define (fn-for-blob b)

	(cond	[(string=? b "bubble") (...)]

		[(string=? b  "solid") (...)]))





;; lob is one of:

;; - empty

;; - (cons blob lob)



(define LOB1 empty)

(define LOB2 (cons B empty))

(define LOB3 (cons S empty))

(define LOB4 (cons B (cons B empty)))

(define LOB5 (cons B (cons S empty)))

(define LOB6 (cons S (cons B empty)))

(define LOB7 (cons S (cons S empty)))



(define (fn-for-lob lob)

	(cond 	[(empty? lob) (...)]

		[else 

		(...	(fn-for-blob (first lob))

			(fn-for-lob  (rest  lob)))]))





;; lob -> lob

(check-expect (sink 	(cons "solid" 

				(cons "solid" 

					(cons "bubble" 

						(cons "bubble" 

							(cons "solid" 

								(cons "solid" 

									(cons "bubble" 

										(cons "bubble" empty)))))))))

			(cons "bubble" 

				(cons "solid" 

					(cons "solid"

						(cons "bubble" 

							(cons "bubble"

								(cons "solid" 

									(cons "solid"

										(cons "bubble" empty)))))))))	

(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty)))) (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink empty) empty)

(check-expect (sink (cons "solid" 

			(cons "solid" 

				(cons "bubble" 

					(cons "solid" empty))))) 

			(cons "bubble"

				(cons "solid" 

					(cons "solid"

						(cons "solid" empty)))))

(check-expect (sink (cons "bubble"

			(cons "solid" 

				(cons "bubble" 

					(cons "solid" empty))))) 

			(cons "bubble"

				(cons "bubble"

					(cons "solid"

						(cons "solid" empty)))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" (cons "solid"
                                                                    (cons "solid"
                                                                          (cons "bubble" (cons "bubble" empty)))))))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" (cons "bubble" (cons "solid" 
                                                                                     (cons "solid" (cons "bubble" empty)))))))))
(check-expect (sink (cons "solid" empty)) (cons "solid" empty))
(check-expect (sink (cons "bubble" empty)) (cons "bubble" empty))
(check-expect (sink (cons "solid" (cons "bubble" empty))) (cons "bubble" (cons "solid" empty)))
(check-expect (sink (cons "bubble" (cons "solid" empty))) (cons "bubble" (cons "solid" empty)))





(define (sink lob)

	(cond 	[(empty? lob) lob]

		[else 

		(cond	[(bubble? (first lob)) (cons "bubble" (sink (rest lob)))]

			[(solid? (first lob)) (insert (first lob) (sink (rest lob)))])]))




#;

;; lob -> lob

;; template for sink

(define (sink lob)

	(cond 	[(empty? lob) (...)]

		[else 

		(...	(fn-for-blob (first lob))

			(sink (rest lob)))]))



;; blob lob -> lob

;; ASSUME: lob parameter is already sunk

(check-expect (insert S LOB1) LOB3)		; S empty -> S

(check-expect (insert S LOB2) LOB5)		; S B -> BS

(check-expect (insert S LOB3) LOB7)		; S S -> SS

(check-expect (insert S LOB4) (cons B LOB6))	; S BB -> BSB

(check-expect (insert S LOB5) (cons B LOB7))	; S BS -> BSS

(check-expect (insert S LOB6) (cons S LOB6))	; S SB -> SSB

(check-expect (insert S LOB7) (cons S LOB7))	; S SS -> SSS

;; summary of check-expects: if b = S and (first lob) = B, return BS + (rest lob), else S + lob



(define (insert b lob)

	(cond 	[(empty? lob) (cons b empty)]

		[else 

		(cond	[(and (solid? b) (bubble? (first lob)))

				(cons B (cons S (rest lob)))]

			[else (cons S lob)])]))



;; blob -> bool

;; true if solid

(define (solid? b)

	(cond	[(string=? b "bubble") false]

		[(string=? b  "solid") true]))



;; blob -> bool

;; true if bubble

(define (bubble? b)

	(cond	[(string=? b "bubble") true]

		[(string=? b  "solid") false]))