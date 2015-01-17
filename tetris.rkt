;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problemset6 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Libraries
(require 2htdp/image)
(require 2htdp/universe)
(require racket/bool)
;;; Constants
(define PIXELS/GRID 10)
(define BOARD-WIDTH 10) ; in grid cells
(define BOARD-HEIGHT 20) ; in grid cells
(define BOARD 
  (empty-scene (* PIXELS/GRID BOARD-WIDTH)
               (* PIXELS/GRID BOARD-HEIGHT)))
;;; grid coordinates have the origin in the bottom-left
;;; and the x-axis goes to the right; the y-axis goes up


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
 
;; A Block is a (make-block size-x size-y Color)
(define-struct block (x y color))

;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))

(define pile1 (list ))
(define pile2 (list (make-block 4 18 "green")))



(define basetet (make-tetra (make-posn 2 2) (list )))
(define testtet (make-tetra (make-posn 2 2) (list (make-block 2 2 "blue"))))

; O tetra 
(define Otet (make-tetra (make-posn 4 18) (list (make-block 4 18 "green") 
                                                (make-block 4 19 "green")
                                                (make-block 5 18 "green")
                                                (make-block 5 19 "green"))))
; I tetra
(define Itet (make-tetra (make-posn 5 19) (list (make-block 4 19 "blue") 
                                                (make-block 5 19 "blue") 
                                                (make-block 6 19 "blue")
                                                (make-block 7 19 "blue"))))
; L tetra
(define Ltet (make-tetra (make-posn 5 18) (list (make-block 4 18 "purple") 
                                                (make-block 5 18 "purple") 
                                                (make-block 6 18 "purple")
                                                (make-block 6 19 "purple"))))
; J tetra 
(define Jtet (make-tetra (make-posn 5 18) (list (make-block 4 18 "lightblue") 
                                                (make-block 4 19 "lightblue") 
                                                (make-block 5 18 "lightblue")
                                                (make-block 6 18 "lightblue"))))
; T tetra
(define Ttet (make-tetra (make-posn 5 19) (list (make-block 4 18 "orange") 
                                                (make-block 5 18 "orange") 
                                                (make-block 5 19 "orange")
                                                (make-block 6 18 "orange"))))
; Z tetra
(define Ztet (make-tetra (make-posn 5 19) (list (make-block 6 18 "pink")
                                                (make-block 4 19 "pink") 
                                                (make-block 5 18 "pink") 
                                                (make-block 5 19 "pink"))))
; S tetra
(define Stet (make-tetra (make-posn 5 19) (list (make-block 4 18 "red") 
                                                (make-block 5 18 "red") 
                                                (make-block 5 19 "red")
                                                (make-block 6 19 "red"))))
;; rand-tetra : Number -> Tetra
;; randomizes the output of any tetra
(define (rand-tetra num)
  (cond [(= num 0) Otet]
        [(= num 1) Itet]
        [(= num 2) Ltet]
        [(= num 3) Jtet]
        [(= num 4) Ttet]
        [(= num 5) Ztet]
        [(= num 6) Stet]))
                                                        
;;place-image/grid : Image Number Number Image -> Image
;; Just like place-image, except in grid coordinates
(define (place-image/grid img1 x y img2)
  (place-image img1
               (* (+ x 1/2) PIXELS/GRID)
               (* (- BOARD-HEIGHT (+ y 1/2)) PIXELS/GRID)
               img2))
           
;; block+img : Block Image -> Image
;; Takes in a single block and an image and returns the combined image
(define (block+img block img) 
  (place-image/grid (overlay (square 10 "outline" "black")
                             (square 10 "solid" (block-color block)))
                    (block-x block)
                    (block-y block) img))

;; tetra+img : Tetra Image -> Image
;; Takes in a tetra and an image and combines them 
(define (tetra+img tetra img)
  (foldr block+img img (tetra-blocks tetra)))

(check-expect (tetra+img basetet BOARD) BOARD)
(check-expect (tetra+img testtet BOARD) (place-image/grid (overlay (square 10 "outline" "black")
                                                               (square 10 "solid" "blue" ))
                                                      2 2 BOARD))

;; pile+img : BSet Image -> Image
;; Just like tetra+img but takes in a pile (a bset)
(define (pile+img pile img)
  (foldr block+img img pile))

(check-expect (pile+img pile1 BOARD) BOARD)
(check-expect (pile+img pile2 BOARD) 
              (place-image/grid (overlay (square 10 "outline" "black")
                                         (square 10 "solid" "green" ))
                                4 18 BOARD))

;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;Base initial world uses random function
(define initial-world (make-world (rand-tetra (random 6)) pile1))

;; posn=? : Number Number Number Number -> Boolean
;; return true when two numbers (x or y) are equal
(check-expect (posn=? 4 4 18 18) true)
(check-expect (posn=? 3 4 21 24) false)

(define (posn=? x1 x2 y1 y2)
  (and (= x1 x2)
       (= y1 y2)))

;;side-collide? : BSet -> Boolean
;; is the block colliding with the side of the board?
(check-expect (side-collide? (cons (make-block 5 19 "pink") 
                                   (cons (make-block 6 19 "pink") empty))) false)
(check-expect (side-collide? (cons (make-block 10 18 "green") 
                                   (cons (make-block 10 17 "green") empty))) true)


(define (side-collide? bset)
  (ormap (lambda (set) (not (and (>= (block-x set) 0) (< (block-x set) BOARD-WIDTH)))) bset))

;; floor-collide? : World -> Boolean
;; Recursively checks if the next world tetra will go beneath the floor 

(check-expect (floor-collide? 
               (make-world (make-tetra (make-posn 4 18) 
                                                      (cons (make-block 4 18 "red") 
                                                            (cons (make-block 5 18 "red") empty))) 
                                          (cons (make-block 2 1 "green") 
                                                (cons (make-block 2 0 "green") empty)))) false)
(check-expect (floor-collide? 
               (make-world (make-tetra (make-posn 5 2) 
                                                      (cons (make-block 5 0 "pink") 
                                                            (cons (make-block 5 1 "pink") 
                                                                  (cons (make-block 5 2 "pink") 
                                                                        empty)))) 
                                          (cons (make-block 2 1 "green") 
                                                (cons (make-block 2 0 "green") empty)))) true)

(define (floor-collide? world)
  (ormap (lambda (block) (< (-(block-y block) 1) 0)) (tetra-blocks (world-tetra world))))
    
;; pile-collide-helper2? : Number Number BSet -> Boolean
;; Helper function for the first helper function, 
;; checks value of x and y with all blocks in second set
(define (pile-collide-helper2? numx numy bset2)
  (ormap (lambda (block) (and (= numx (block-x block)) (= numy (block-y block)))) bset2))
                   
;; pile-collide-helper? : BSet BSet -> Boolean
;; Helper function for pile-collide? Checks through all blocks of first set
(define (pile-collide-helper? bset1 bset2)
  (ormap (lambda (block1) (pile-collide-helper2? (block-x block1) (- (block-y block1) 1) bset2)) bset1)) 

;; pile-collide? : World -> Boolean
;; Takes in a world and returns if the next tetra down will collide with a pile
(check-expect (pile-collide? (make-world (make-tetra (make-posn 4 18) 
                                                     (cons (make-block 4 18 "red") 
                                                           (cons (make-block 5 18 "red") empty))) 
                                         (cons (make-block 2 1 "green") 
                                               (cons (make-block 2 0 "green") empty)))) false)
(check-expect (pile-collide? (make-world (make-tetra (make-posn 5 2) 
                                                     (cons (make-block 5 0 "pink") 
                                                           (cons (make-block 5 1 "pink") 
                                                                 (cons (make-block 5 2 "pink") 
                                                                       empty)))) 
(cons (make-block 5 1 "green") (cons (make-block 5 0 "green") empty)))) true)

(define (pile-collide? world)
  (pile-collide-helper? (tetra-blocks (world-tetra world)) (world-pile world)))

;; left-side-pile-collide? : BSet BSet -> Boolean
;; Takes in two sets and returns whether the next tetra to the left will hit a pile
(define (left-side-pile-collide? bset1 bset2)
  (ormap (lambda (block1) 
           (pile-collide-helper2? (- (block-x block1) 1)
                                  (block-y block1) 
                                  bset2)) bset1))

(check-expect (left-side-pile-collide? (cons (make-block 7 16 "blue") 
                                             (cons (make-block 7 18 "blue") empty))
(cons (make-block 6 16 "green") (cons (make-block 5 16 "green") empty))) true)
(check-expect (left-side-pile-collide? (cons (make-block 4 18 "red") 
                                             (cons (make-block 4 19 "red") empty))
(cons (make-block 2 2 "pink") (cons (make-block 2 3 "pink") empty))) false)

;; right-side-pile-collide? : BSet BSet -> Boolean
;; Takes in two sets and returns if the next tetra to the right will hit a pile
(define (right-side-pile-collide? bset1 bset2)
  (ormap (lambda (block1) 
           (pile-collide-helper2? (+ (block-x block1) 1)
                                  (block-y block1) 
                                  bset2)) bset1))

(check-expect (right-side-pile-collide? (cons (make-block 7 16 "blue") 
                                             (cons (make-block 7 18 "blue") empty))
(cons (make-block 8 16 "green") (cons (make-block 8 16 "green") empty))) true)

(check-expect (right-side-pile-collide? (cons (make-block 4 18 "red") 
                                             (cons (make-block 4 19 "red") empty))
(cons (make-block 2 2 "pink") (cons (make-block 2 3 "pink") empty))) false)

;; left-tetra : BSet -> BSet
;; Returns the next block set to the left
(define (left-tetra bset)
  (map (lambda (block) (make-block (- (block-x block) 1)
                                  (block-y block)
                                  (block-color block))) bset))

(check-expect (left-tetra (tetra-blocks basetet)) (list ))
(check-expect (left-tetra (tetra-blocks testtet)) (list (make-block 1 2 "blue")))


;; right-tetra : BSet -> BSet
;; Returns the next block set to the right
(define (right-tetra bset)
  (map (lambda (block) (make-block (+ (block-x block) 1)
                                  (block-y block)
                                  (block-color block))) bset))
(check-expect (right-tetra (tetra-blocks basetet)) (list ))
(check-expect (right-tetra (tetra-blocks testtet)) (list (make-block 3 2 "blue")))


;; gravity : tetra -> tetra
;; Takes in a tetra and subracts 1 from y value to bring it down a block
(define (gravity tetra)
  ;; gravity-help : BSet -> BSet
  ;; Helper function for gravity, recursively subtracts 1 from y valeu from each set in Bset     
  (local ((define (gravity-help bset)
            (map (lambda (block) (make-block (block-x block)
                                   (- (block-y block) 1)
                                   (block-color block))) 
                 bset)))
    (make-tetra (make-posn (posn-x (tetra-center tetra)) (-(posn-y (tetra-center tetra ))1))
                (gravity-help (tetra-blocks tetra))))) 

;; pile-hit-world : World -> World
;; Takes in a world and returns the new world with the old tetra combined with the old pile
(define (pile-hit-world w)
  (make-world (rand-tetra (random 6)) (append (tetra-blocks (world-tetra w)) (world-pile w))))

;; score+img : num img -> img 
;; Takes in number for score and background image and combines them
(define (score+img num img)
  (place-image/grid (text (string-append "Score: " (number->string num)) 12 "red") 3 18 img))

;; score world: world -> number
;; Takes in a world and finds the total number of blocks in pile
(define (score world)
  (cond [(empty? (world-pile world)) 0]
        [else (+ 1 (score (make-world (world-tetra world) (rest (world-pile world)))))]))

;; fullrow : number -> [list-of blocks]
;; returns a full row of blocks for that y value
(define (fullrow y) (list (make-block 0 y "red")
                          (make-block 1 y "red")
                          (make-block 2 y "red")
                          (make-block 3 y "red")
                          (make-block 4 y "red")
                          (make-block 5 y "red")
                          (make-block 6 y "red")
                          (make-block 7 y "red")
                          (make-block 8 y "red")
                          (make-block 9 y "red")))


;; contains? : [set-of blocks] block -> boolan
;; checks to see if that block is in that set of blocks
(define (contains? set elts)
  (ormap (lambda (e) (and (equal? (block-x e) (block-x elts))
                          (equal? (block-y e) (block-y elts)))) set))

(check-expect (contains? (list (make-block 2 3 "red") (make-block 3 4 "blue")) (make-block 2 3 "blue")) true)
(check-expect (contains? (list (make-block 2 3 "red") (make-block 3 4 "blue")) (make-block 2 4 "blue")) false)

;;set = list of anything, regardless of order

;; elt+set : element set -> set
;; adds an element to a set
(define (elt+set e s)
  (cond [(contains? s e) s]
        [else (cons e s)]))

;; subset? : set set -> boolean
;; checks to see if the first set isa subset of the second
(define (subset? a b)
  (andmap (lambda (a-elt) (contains? b a-elt)) a))

(check-expect (subset? (list (make-block 2 3 "red")) (list (make-block 2 5 "yellow") (make-block 2 3 "blue"))) true)

;; union : set set -> set
;; returns the combined union set, ignores repetition
(define (union a b)
  (foldr elt+set b a))

(check-expect (union (list (make-block 2 3 "red")) (list (make-block 3 4 "blue"))) (list (make-block 2 3 "red") (make-block 3 4 "blue")))

;; list of numbers from 0 - 19 
(define list0->19 (map (lambda (x) (- x 1)) (build-list 20 (lambda (x) (+ x 1)))))

;; check-row? : world -> boolean
;; Checks to see if there is a row you can remove
(define (check-row? world)
  (ormap (lambda (x) (subset? (fullrow x) (world-pile world))) (map (lambda (x) (- x 1)) (build-list 20 (lambda (x) (+ x 1))))))

(check-expect (check-row? (make-world Otet (fullrow 2))) true)
(check-expect (check-row? (make-world Otet (list (make-block 0 1 "red")
                                                 (make-block 1 2 "red")
                                                 (make-block 2 3 "red")
                                                 (make-block 3 4 "red")
                                                 (make-block 4 5 "red")
                                                 (make-block 5 1 "red")
                                                 (make-block 6 1 "red")
                                                 (make-block 7 1 "red")
                                                 (make-block 8 1 "red")
                                                 (make-block 9 1 "red")))) false)
;; check-row= : world [list-of numbers] -> number
;; returns the y value of a complete row
(define (check-row= world y)
  (cond [(empty? y) empty]
        [(subset? (fullrow (first y)) (world-pile world)) (first y)]
        [else (check-row= world (rest y))]))

(check-expect (check-row= (make-world Otet (fullrow 2)) list0->19 ) 2)
(check-expect (check-row= (make-world Otet (list (make-block 0 1 "red")
                                                 (make-block 1 2 "red")
                                                 (make-block 2 3 "red")
                                                 (make-block 3 4 "red")
                                                 (make-block 4 5 "red")
                                                 (make-block 5 1 "red")
                                                 (make-block 6 1 "red")
                                                 (make-block 7 1 "red")
                                                 (make-block 8 1 "red")
                                                 (make-block 9 1 "red"))) list0->19 ) empty)

(check-expect (subtract-set (list (make-block 2 3 "red")
                                  (make-block 3 3 "red")
                                  (make-block 4 3 "red")
                                  (make-block 5 3 "red")
                                  (make-block 7 3 "red")
                                  (make-block 8 3 "red")
                                  (make-block 9 3 "red")) 3) empty)
                                  
(check-expect (subtract-set (list (make-block 2 3 "red")
                                  (make-block 3 3 "red")
                                  (make-block 4 3 "red")
                                  (make-block 5 3 "red")
                                  (make-block 7 3 "red")
                                  (make-block 8 3 "red")
                                  (make-block 9 2 "red")) 3) (list (make-block 9 2 "red")))
;; subtract-set : [set-of blocks] number -> [set-of blocks]
;; takes a set of blocks and a y integer for a row, and removes all blocks matching with that y
(define (subtract-set set y)
  (cond [(empty? set) empty]
        [(= (block-y (first set)) y) (subtract-set (rest set) y) ]
        [else (cons (first set) (subtract-set (rest set) y))]))
 
;; gravity-help : block -> block
;; subtract 1 from y
(define (gravity-help block)
            (make-block (block-x block)
                                   (- (block-y block) 1)
                                   (block-color block)))                  

(check-expect (move-row (fullrow 1) 0) (fullrow 0))

;; move-row : [set-of blocks] number -> [set-of blocks]
;; returns the blocks above the number, set down by 1 to move the blocks down after removal
(define (move-row set y)
  (cond [(empty? set) empty]
        [(> (block-y (first set)) y) (cons (gravity-help (first set)) (move-row (rest set) y))]
        [else (cons (first set) (move-row (rest set) y))]))

          
;; remove-row : world->world
;; removes a row and moves it down
(define (remove-row world)
  (make-world (world-tetra world) (move-row (subtract-set (world-pile world) (check-row= world list0->19)) (check-row= world list0->19))))
  
        
;; next-world : World -> World
;; Takes in a world and checks logical functions and returns new world
(define (next-world world)
  (cond [(pile-collide? world) (pile-hit-world world)]
        [(side-collide? (tetra-blocks (world-tetra world))) 
         (make-world (gravity (world-tetra world)) (world-pile world))]
        [(floor-collide? world) (pile-hit-world world)]
        [(check-row? world) (remove-row world)]
        [else (make-world (gravity (world-tetra world)) (world-pile world))]))
  

;; world->image : World -> Image
;; draws the world (score and tetra and pile)
(define (world->image world)
  (score+img (score world) (tetra+img (world-tetra world)
             (pile+img (world-pile world) BOARD))))
             
;; rotate-ccw : center bset -> bset
;; takes in center as posn and set of blocks and returns set of blocks rotated counterclockwise                       
(define (rotate-ccw center bset )
  (cond [(empty? bset) empty]
        [else (cons (block-rotate-ccw center (first bset))
                                                     (rotate-ccw center (rest bset)))]))
;; rotate-cw : center bset -> bset
;; takes in center as posn and set of blocks and returns set of blocks rotated clockwise                                                        
(define (rotate-cw center bset )
  (cond [(empty? bset) empty]
        [else (cons (block-rotate-cw center (first bset))
                                                     (rotate-cw center (rest bset)))]))         

;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))
              
;; block-rotate-cw : Posn Block -> Block
;; Rotate the block 90 clockwise around the posn.
(define (block-rotate-cw c b)
  (make-block (+ (posn-x c)
                 (- (block-y b)
                    (posn-y c)))
              (+ (posn-y c)
                 (- (posn-x c)
                    (block-x b)))
              (block-color b)))

;; overflow? : world -> boolean
;; Checks if the pile y is higher then the empty scene to end game.
(define (overflow? world)
  (ormap (lambda (block) (>= (block-y block) 19)) (world-pile world)))

;; key-handler: World Key -> World
;; compute the new state of the world after a key press
(define (key-handler w k)
  (cond [(and 
          (key=? k "left")
          (not (side-collide? (left-tetra (tetra-blocks (world-tetra w)))))
          (not (left-side-pile-collide? (tetra-blocks (world-tetra w)) (world-pile w))))
         (make-world (make-tetra (make-posn (- (posn-x (tetra-center (world-tetra w))) 1) 
         (posn-y (tetra-center (world-tetra w)))) 
         (left-tetra (tetra-blocks (world-tetra w)))) (world-pile w))]
        [(and 
          (key=? k "right")
          (not (side-collide? (right-tetra (tetra-blocks (world-tetra w)))))
          (not (right-side-pile-collide? (tetra-blocks (world-tetra w)) (world-pile w))))
         (make-world (make-tetra (make-posn (+ (posn-x (tetra-center (world-tetra w))) 1) 
         (posn-y (tetra-center (world-tetra w)))) 
                                 (right-tetra (tetra-blocks (world-tetra w)))) (world-pile w))]
        [(and
          (key=? k "a")
          (not (side-collide? (rotate-ccw (tetra-center (world-tetra w)) 
          (tetra-blocks (world-tetra w)))))
          (not (floor-collide? (make-world (make-tetra (tetra-center (world-tetra w)) 
          (rotate-ccw (tetra-center (world-tetra w)) 
                      (tetra-blocks (world-tetra w)))) (world-pile w))))        
          (not (right-side-pile-collide? 
          (rotate-ccw (tetra-center (world-tetra w)) 
                      (tetra-blocks (world-tetra w))) (world-pile w)))
          (not (left-side-pile-collide? (rotate-ccw (tetra-center (world-tetra w)) 
                                                    (tetra-blocks (world-tetra w))) 
                                        (world-pile w))))
         (make-world (make-tetra (tetra-center (world-tetra w)) 
                                 (rotate-ccw (tetra-center (world-tetra w)) 
                                             (tetra-blocks (world-tetra w)))) (world-pile w))]
        [(and
          (key=? k "s")
          (not (side-collide? (rotate-cw (tetra-center (world-tetra w)) 
                                         (tetra-blocks (world-tetra w)))))
          (not (floor-collide? (make-world (make-tetra (tetra-center (world-tetra w)) 
                                                       (rotate-cw (tetra-center (world-tetra w)) 
                                                                  (tetra-blocks (world-tetra w)))) 
                                           (world-pile w))))        
          (not (right-side-pile-collide? (rotate-cw (tetra-center (world-tetra w)) 
                                                    (tetra-blocks (world-tetra w))) (world-pile w)))
          (not (left-side-pile-collide? (rotate-cw (tetra-center (world-tetra w)) 
                                                   (tetra-blocks (world-tetra w))) (world-pile w))))
         (make-world (make-tetra (tetra-center (world-tetra w)) 
                                 (rotate-cw (tetra-center (world-tetra w)) 
                                            (tetra-blocks (world-tetra w)))) (world-pile w))]        
        [else w]))

(big-bang
 initial-world
 (on-tick next-world 1/3)
 (to-draw world->image)
 (on-key key-handler)
 (stop-when overflow?))


  
  