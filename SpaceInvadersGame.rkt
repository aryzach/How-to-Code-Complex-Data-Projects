;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |space invaders 4-30|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
 
;; Space Invaders
 
 
;; Constants:
 
(define WIDTH  300)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))
 
(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
 
(define HIT-RANGE 10)
 
(define INVADE-RATE 100)
 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
 
(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
 
(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
 
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))
 
(define MISSILE (ellipse 5 15 "solid" "red"))
 
 
 
;; Data Definitions:
 
(define-struct game (invaders missiles tank)) ;; Game is (make-game  (listof Invader) (listof Missile) Tank) ;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
;; Game constants defined below Missile data definition
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))
 
 
 
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1]) ;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))
 
 
 
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number) ;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick
(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))
 
 
(define-struct missile (x y))
;; Missile is (make-missile Number Number) ;; interp. the missile's location is x y in screen coordinates
(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1 (define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))
 
 
 
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
 
 
;; Functions:
 
;; game -> game
;; start the world with (main G0)
;;
(define (main game)
  (big-bang game           ; game
    (on-tick   tock)       ; game -> game
    (to-draw   render)     ; game -> Image
    (stop-when invaderwin?); game -> Boolean
    (on-key    3keys)))    ; game KeyEvent -> game
 
 
 
;; game -> game
;; tick invaders, missles, and the tank. increase positions and remove bullets as they leave the screen, and remove invaders and bullets when they are within HIT-RANGE ;; !!!
(check-expect (tock G0) (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
;(define (tock game) game) ;stub
(define (tock g)
  (remove-out (increment g)))
 
 
;; game -> Image
;; render each data type individually, but they will have to be rendered in consession (place on top of eachother) to form the whole scene
;; !!!
;; maybe I should use overlay instead of rendering on top of each other?
;(define (render game) img) ;stub
(define (render g)
  (render-loi (game-invaders g)
              (render-lom (game-missiles g) (render-tank (game-tank g)))))
 
;; game -> Boolean
;; if at least one invader-y > HEIGHT, return true
;; !!!
;(define (invaderwin? game) false) ;stub
(define (invaderwin? g)
  (invader-more-than-HEIGHT? (game-invaders g)))
 
 
;; game KeyEvent -> game
;; if left arrow pressed, change tank-dir to -1, if right, to 1. If space is pressed, add bullet to bullet list at bullet-x = (image-height TANK) and bullet-y = tank-y
;; !!!
;(define (3keys game ke) game) ;stub
(define (3keys g ke)
  (cond [(key=? ke " ") (add-missile g)]
        [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [else g]))
 
 
;; tock helpers
 
;; game -> game
;; increment game
;;!!!
;(define (increment g) g) ;stub
(define (increment g)
  (make-game    (increment-add-loi (game-invaders g))
                (increment-lom (game-missiles g))
                (increment-tank (game-tank g))))
 
;; game -> game
;; remove missiles that are off screen or have collided with invaders and remove invaders that have collided with missiles
;; !!!
;(define (remove g) g) ;stub
(define (remove-out g)
  (make-game    (remove-loi (game-invaders g) (game-missiles g))
                (remove-lom (game-missiles g))
                (game-tank g)))
 
 
;; tock-increment helpers
 
;; loi and invader
;; (done)
 
;; loi -> loi
;; tock position and possibly direction of each invader in list, add new invader to list at INVADE-RATE (not sure how to do this last part)
;(define (increment-loi loi) loi) ;stub
(define (increment-loi loi)
  (cond    [(empty? loi) loi]
           [else
            (cons (increment-invader (first loi)) (increment-loi (rest loi)))]))

;; loi -> loi
;; if random = INVADE-RATE, add invader, else, increment-loi
(define (increment-add-loi loi)
  (cond [(= (random (+ INVADE-RATE 1)) INVADE-RATE) (cons (make-invader (random WIDTH) 0 (random 16)) (increment-loi loi))]
        [else (increment-loi loi)]))
 
;; invader -> invader
;; increment one invader
(check-expect (increment-invader (make-invader 150 100 12)) (make-invader (+ 12 150) (+ INVADER-Y-SPEED 100) 12))
(check-expect (increment-invader (make-invader WIDTH 100 12)) (make-invader (+ -12 WIDTH) (+ INVADER-Y-SPEED 100) -12))
(check-expect (increment-invader (make-invader 2 10 -12)) (make-invader 10 (+ INVADER-Y-SPEED 10) 12))
;(define (increment-invader i) i) ;stub
(define (increment-invader i)
  (cond    [(turn-invader? i) (make-invader
                               (turn-invader-x i) (+ INVADER-Y-SPEED (invader-y i)) (* -1 (invader-dx i)))]
           [else (make-invader
                  (+ (invader-x i) (invader-dx i)) (+ INVADER-Y-SPEED (invader-y i)) (invader-dx i))]))
 
;; invader -> bool
;; true if invader bounce off wall on tick
(check-expect (turn-invader? (make-invader 2 10 -12)) true)
(check-expect (turn-invader? (make-invader 2 10 12)) false)
(check-expect (turn-invader? (make-invader WIDTH 100 12)) true)
;(define (turn-invader? i) false) ;stub
(define (turn-invader? i)
  (cond     [(> 0 (+ (invader-x i) (invader-dx i))) true]
            [(< WIDTH (+ (invader-x i) (invader-dx i))) true]
            [else false]))
 
;; invader -> Integer
;; calculate invader-x for an invader that's changing directions
(define (turn-invader-x i)
  (cond     [(< 0 (invader-dx i)) (- WIDTH (- (invader-dx i) (- WIDTH (invader-x i))))]
            [(>= 0 (invader-dx i)) (* -1 (+ (invader-x i) (invader-dx i)))]))
 
 
            ;; lom and missile
 
            ;; lom -> lom
            ;; tock position and possibly direction of each missile in list
            ;; !!!
            ;(define (increment-lom lom) lom) ;stub
            (define (increment-lom lom)
              (cond    [(empty? lom) lom]
                       [else
                        (cons (increment-missile (first lom)) (increment-lom (rest lom)))]))
 
            ;; missile -> missile
            ;; increment one missile
            ;(define (increment-missile m) m) ;stub
            (define (increment-missile m)
              (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))
 
            ;; tank
 
            ;; tank -> tank
            ;; increment position of tank by TANK-SPEED by direction given by tank-dir. If at a wall (tank-x is 0 or WIDTH) stay there
            (check-expect (increment-tank (make-tank 0 -1)) (make-tank 0 -1))
            (check-expect (increment-tank (make-tank WIDTH -1)) (make-tank (- WIDTH TANK-SPEED) -1))
            (check-expect (increment-tank (make-tank 0 1)) (make-tank (+ 0 TANK-SPEED) 1))
            (check-expect (increment-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
            ;(define (increment-tank t) t) ;stub
            (define (increment-tank t)
              (cond [(>= (do-inc-tank t) WIDTH) (make-tank WIDTH (tank-dir t))]
                    [(<= (do-inc-tank t)     0) (make-tank     0 (tank-dir t))]
                    [else (make-tank (do-inc-tank t) (tank-dir t))]))
                     
            ;; tank -> Integer
            ;; calculate tank x increment without stopping tank at 0 or width
            (check-expect (do-inc-tank (make-tank 5 1)) (+ TANK-SPEED 5))
            (check-expect (do-inc-tank (make-tank 5 -1)) (- 5 TANK-SPEED ))
            (check-expect (do-inc-tank (make-tank WIDTH 1)) (+ WIDTH TANK-SPEED))
            (define (do-inc-tank t)
              (+ (* (tank-dir t) TANK-SPEED) (tank-x t)))
 
 
            ;; tock-remove helpers
 
            ;; loi lom -> loi
            ;; remove invaders that are within HIT-RANGE of any missile, or y > HEIGHT
            ;; !!!
            ;(define (remove-loi loi lom) loi) ;stub
            (define (remove-loi loi lom)
              (cond [(empty? loi) empty]
                    [else
                     (cond [(hit-one-inv? (first loi) lom) (remove-loi (rest loi) lom)]
                           [else (cons (first loi) (remove-loi (rest loi) lom))])]))

            ;; invader lom -> bool
            ;; compare distance of one invader to all missiles. If hit?, return true
            (define (hit-one-inv? i lom)
              (cond [(empty? lom) false]
                    [else
                     (cond [(hit? i (first lom)) true]
                           [else (hit-one-inv? i (rest lom))])]))
 
            ;; lom -> lom
            ;; remove missiles that are off screen or within HIT-RANGE of any invader
            ;; !!!
            ;(define (remove-lom lom) lom) ;stub
            (define (remove-lom lom)
              (cond [(empty? lom) empty]
                    [else
                     (cond [(< (missile-y (first lom)) 0) (remove-lom (rest lom))]
                           [else (cons (first lom) (remove-lom (rest lom)))])]))
                     
 
            ;; render helpers
 
            ;; tank -> Image
            ;; place tank at current position on top of MTS
            ;; !!!
            ;(define (render-tank t) MTS) ;stub
            (define (render-tank t)
              (place-image TANK (tank-x t) TANK-Y MTS))
 
            ;; lom Image -> Image
            ;; place all missiles on top of the rendered tank on MTS. Input image is the rendered tank
            ;; !!!
            ;(define (render-lom lom MTS) MTS) ;stub
            (define (render-lom lom img)
              (cond [(empty? lom) (place-image (square 0 "solid" "white") 0 0 img)]
                    [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-lom (rest lom) img))])) 
                   
              
 
            ;; loi Image -> Image
            ;; place all invaders on on the rendered missiles on tank on MTS. Input image is the rendered missiles on tank
            ;; !!!
            ;(define (render-loi loi MTS) MTS) ;stub
            (define (render-loi loi img)
              (cond [(empty? loi) (place-image (square 0 "solid" "white") 0 0 img)]
                    [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-loi (rest loi) img))]))
            
            ;; invaderwin? helper
 
            ;; loi -> bool
            ;; if any invader-y in loi > HEIGHT, return true
            ;(define (invader-more-than-HEIGHT? loi) false) ;stub
            (define (invader-more-than-HEIGHT? loi)
              (cond [(empty? loi) false]
                    [else
                     (cond [(>= (invader-y (first loi)) HEIGHT) true]
                           [else (invader-more-than-HEIGHT? (rest loi))])]))
              
            ;; game -> game
            ;; add missle to lom
            ;; !!!
            ;(define (add-missile g) g) ;stub
            (define (add-missile g)
              (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) TANK-Y) (game-missiles g)) (game-tank g)))
 
            ;; invader missile -> bool
            ;; if the two are within HIT-RANGE from each other, return true
            ;(define (distance I1 G2) false) ;stub
            (define (hit? i m)
              (cond [(< (sqrt (+ (sqr (- (invader-x i) (missile-x m))) (sqr (- (invader-y i) (missile-y m))))) HIT-RANGE) true]
                    [else false]))
              
 
 
 
 
            