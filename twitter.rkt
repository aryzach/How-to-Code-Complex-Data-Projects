;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname twitter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; final problem 1
;; twitter followers
 
;; template for arbitrary-arity tree w/ acc for places already visited
;;
 
 
(define-struct user (name v follows))
;; user is (make-user String String (listof user))
;; interp. name as the persons name, v as "y" or "n" for varified, and follows as a list of users that they follow

(define T1 (shared ((-A- (make-user "ok" "y" empty))
                    (-B- (make-user "g" "y" (list -A-)))
                    (-C- (make-user "b" "n" (list -A- -B-))))
             -C-))

(check-expect (max-follow T1)
              (shared ((-A- (make-user "ok" "y" empty))
                       (-B- (make-user "g" "y" (list -A-)))
                       (-C- (make-user "b" "n" (list -A- -B-))))
                -A-))
 
(define (max-follow user)
  (local [(define-struct userNfollowers (user f))
          ;; acc: visted is (listof user); list of users that we have already seen
          ;; acc: todo is a worklist accumulator; all the users that are followed that we have come across but not operated on yet
          ;; acc: lounf is (listof userNfollowers); a list of users and the number of followers they have
          (define (max-follow user visited todo lounf)
            (if (member user visited)
                (fn-for-todo visited todo lounf)
                (fn-for-todo
                 (cons user visited)
                 (append (user-follows user) todo)
                 (update-lounf (user-follows user) lounf))))
          (define (fn-for-todo visited todo lounf)
            (cond [(empty? todo) (who-has-most-followers? lounf)] ;(who-has-most-followers? lounf) replaced visited
                  [else
                   (max-follow (first todo) visited (rest todo) lounf)]))
    
 
 
 
          ;; (listof user) (listof userNfollowers) -> (listof userNfollowers)
          ;; if user is not in (map userNfollowers-user lounf), add it to the list with (userNfollowers-f = 1)
          ;;             if it is in the map, add one to userNfollowers-f
          ;; !!!
          ;(define (update-lounf lou lounf) empty) ;stub
          (define (update-lounf lou lounf)
            (cond [(empty? lou) lounf]
                  [else
                   (if (member (first lou) (map userNfollowers-user lounf))
                       (update-lounf (rest lou) (add1-follower (first lou) lounf))
                       (update-lounf (rest lou) (cons (make-userNfollowers (first lou) 1) lounf)))]))


          ;; User (listof userNfollowers) -> (listof userNfollowers)
          ;; add one follower to the user that matches
          ;; Assume: user is already in lounf             
          (define (add1-follower user lounf)
            (cond [(empty? lounf) (error "user should be in lounf")]
                  [else
                   (if (string=? (user-name user) (user-name (userNfollowers-user (first lounf))))
                       (cons (make-userNfollowers user (add1 (userNfollowers-f (first lounf)))) (rest lounf))
                       (cons (first lounf) (add1-follower user (rest lounf))))]))
  
  
          ;; (listof userNfollowers) -> user
          ;; produce user who has the biggest of (userNfollowers-f)
          ;; !!!
          ;(define (who-has-most-followers? lounf) (make-user "" "" empty))  ;stub
          (define (who-has-most-followers? lounf)
            (local [(define (who? lounf rsf)
                      (cond [(empty? lounf) (userNfollowers-user rsf)] ;replaced (userNfollowers-user rsf) with lounf for testing
                            [else
                             (if (>= (userNfollowers-f (first lounf)) (userNfollowers-f rsf))
                                 (who? (rest lounf) (first lounf))
                                 (who? (rest lounf) rsf))]))]
              (who? lounf (make-userNfollowers (make-user "" "" empty) 0))))]

    (max-follow user empty empty empty)))
  
