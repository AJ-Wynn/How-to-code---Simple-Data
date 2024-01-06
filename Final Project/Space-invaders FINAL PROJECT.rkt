;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Space-invaders FINAL PROJECT|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
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

(define MISSILE (ellipse 5 15 "solid" "red"))


;; ============================================================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T3 (make-tank 0 -1))            ;at left wall
(define T4 (make-tank WIDTH 1))         ;at right wall

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 (* INVADER-Y-SPEED 12)) 12))
(define I5 (make-invader (- WIDTH (/ (image-width INVADER) 2)) 250 12))  ;about to hit right wall, moving right
(define I6 (make-invader (- WIDTH (/ (image-width INVADER) 2)) 250 -12)) ;about to hit right wall, moving left
(define I7 (make-invader (+ 0 (/ (image-width INVADER) 2)) 250 12))      ;about to hit left wall, moving right
(define I8 (make-invader (+ 0 (/ (image-width INVADER) 2)) 250 -12))     ;about to hit left wall, moving left
(define I9 (make-invader 150 100 -12))
(define I10 (make-invader 150 300 12))
(define I11 (make-invader 0 0 12))
(define I12 (make-invader 150 (- HEIGHT (/ (image-height INVADER) 2)) 12));about to hit bottom wall



#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 150 (- 300 MISSILE-SPEED)))
(define M5 (make-missile 50 400))
(define M6 (make-missile 200 50))
(define M7 (make-missile 150 0))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game empty empty T3))
(define G5 (make-game empty empty T4))
(define G6 (make-game (list I1 I2 I12) (list M1) T1))


;; ===================================================================
;; Functions:

;; Game -> Game
;; a space invaders game. Start the world with (main G0)
;;
(define (main g)
  (big-bang g 
            (on-tick advance-game)        ; Game -> Game
            (on-key handle-key)           ; Game KeyEvent -> Game
            (to-draw render-game)         ; Game -> Image
            (stop-when game-over?)))      ; Game -> Game Boolean







;; Game -> Game
;; advances the y position of missiles by MISSILE-SPEED, advances position of invader by dx and bounces when they hit a wall
;; //  spawns invaders at random x position, if missile is within HIT-RANGE of invader, remove invader and missile from world
;; tests not appropriate as dealing with random numbers

;(define (advance-game g) G0)  ;stub

;<used template from Game>

(define (advance-game g)
  (if (< (random INVADE-RATE) 4)
      (make-game (advance-invader (spawn-invader (game-invaders g)) (game-missiles g))     ;if (random INVADE-RATE) is < 4, then spawn new invader
                 (advance-missile (game-missiles g) (game-invaders g))
                 (advance-tank (game-tank g)))
      (make-game (advance-invader (game-invaders g) (game-missiles g))                     ;not spawning new invader
                 (advance-missile (game-missiles g) (game-invaders g))
                 (advance-tank (game-tank g)))))
             


;; Tank -> Tank
;; tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1. If at the left or right wall, then tank is stationary
(check-expect (advance-tank T0) (make-tank (+ (/ WIDTH 2) (* 1 TANK-SPEED)) 1))
(check-expect (advance-tank T1) (make-tank (+ 50 (* 1 TANK-SPEED)) 1))
(check-expect (advance-tank T2) (make-tank (+ 50 (* -1 TANK-SPEED)) -1))
(check-expect (advance-tank T3) T3)
(check-expect (advance-tank T4) T4)

;(define (advance-tank t) T0)  ;stub

;<used template from Tank>

(define (advance-tank t)
  (cond [(<= (tank-x t) 0) t]
        [(>= (tank-x t) WIDTH) t]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))



;; ListOfMissile ListOfInvader -> ListOfMissile
;; decreases the y position of every missile in ListOfMissile by MISSILE-SPEED, if missile hits invader then remove missile from list
;; // if missile reaches top of screen, remove from list
(check-expect (advance-missile (list M1) empty) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (advance-missile (list M1 M5 M6) empty) (list (make-missile 150 (- 300 MISSILE-SPEED))
                                                      (make-missile 50 (- 400 MISSILE-SPEED))
                                                      (make-missile 200 (- 50 MISSILE-SPEED))))
(check-expect (advance-missile (list M1 M2) (list I1 I6)) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (advance-missile (list M1 M7) (list I1 I6)) (list (make-missile 150 (- 300 MISSILE-SPEED))))

;(define (advance-missile lom loi) (list M1)  ;stub

;<used template from Missile>

(define (advance-missile lom loi)
   (cond [(empty? lom) empty]
         [(m-collision? (first lom) loi) (advance-missile (rest lom) loi)]     ;if missile collides with invader, remove missile from list
         [(missile-exit? (first lom)) (advance-missile (rest lom) loi)]        ;if missile has reached top of screen, remove missile from list
         [else
          (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                (advance-missile (rest lom) loi))]))



;; ListOfInvader ListOfMissile -> ListOfInvader
;; advances x and y position of invader according to dx, INVADER-X-SPEED and INVADER-Y-SPEED. Invader bounces when it hits a wall
;; // if invader hits missile, remove invader from list
(check-expect (advance-invader (list I1) empty) (list I4))
(check-expect (advance-invader (list I5) empty) (list (make-invader (- WIDTH (/ (image-width INVADER) 2)) 250 -12)))
(check-expect (advance-invader (list I9) empty) (list (make-invader (+ 150 (* -12 INVADER-X-SPEED)) (+ 100 (* (* -12 INVADER-Y-SPEED) -1)) -12)))
(check-expect (advance-invader (list I8) empty) (list (make-invader (+ 0 (/ (image-width INVADER) 2)) 250 12)))

;(define (advance-invader loi lom) (list I1)  ;stub

;<used template from Invader>

(define (advance-invader loi lom)
  (cond [(empty? loi) empty]
        [(i-collision? (first loi) lom) (advance-invader (rest loi) lom)]                         ;if missile collides with invader, remove invader from list
        [(hit-wall? (first loi))                                                                  ;if invader hits wall, change direction
         (cons (make-invader (invader-x (first loi)) (invader-y (first loi)) (- (invader-dx (first loi))))   
                      (advance-invader (rest loi) lom))]
        [else
         (cons (make-invader (+ (invader-x (first loi)) (* (invader-dx (first loi)) INVADER-X-SPEED))
                             (+ (invader-y (first loi)) (* (abs (invader-dx (first loi))) INVADER-Y-SPEED))
                             (invader-dx (first loi)))
               (advance-invader (rest loi) lom))]))
                
               




;; Invader -> Boolean
;; returns true if Invader has hit the left wall and is travelling left / hit the right wall and is travelling right
(check-expect (hit-wall? I5) true)
(check-expect (hit-wall? I6) false)
(check-expect (hit-wall? I7) false)
(check-expect (hit-wall? I8) true)

;(define (hit-wall? i) false)  ;stub

;<used template from Invader>

(define (hit-wall? i)
  (or (and (>= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)))      ;hit right wall and travelling rightward
               (positive? (invader-dx i)))
      (and (<= (invader-x i) (+ 0 (/ (image-width INVADER) 2)))          ;hit left wall and travelling leftward
               (negative? (invader-dx i)))))
      


;; ListOfInvader -> ListOfInvader
;; spawns invaders at random x position, with random dx
;; tests not appropriate as dealing with random numbers

;(define (spawn-invader loi) (list I1))  ;stub

;<used template from Invader>

(define (spawn-invader loi)
  (cons (make-invader (random WIDTH) 0 (+ (random 5) 1))
        loi))




;; Missile ListOfInvader -> Boolean
;; returns true if given missile is within HIT-RANGE of any invader in ListOfInvader
(check-expect (m-collision? M1 (list I1)) false)        ;missile not hit I1
(check-expect (m-collision? M2 (list I1)) true)         ;missile exactly hit I1
(check-expect (m-collision? M3 (list I1)) true)         ;missile >hit I1

;(define (m-collision? m loi) false)  ;stub

;<used template from Missile>

(define (m-collision? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (>= (missile-x m) (- (invader-x (first loi)) HIT-RANGE))
                  (<= (missile-x m) (+ (invader-x (first loi)) HIT-RANGE))
                  (<= (missile-y m) (+ (invader-y (first loi)) HIT-RANGE)))
             true
             (m-collision? m (rest loi)))]))
             


;; Missile -> Boolean
;; returns true if given missile has reached the top of the background
(check-expect (missile-exit? M1) false)
(check-expect (missile-exit? M7) true)

;(define (missile-exit? m) false)  ;stub

;<used template from Missile>

(define (missile-exit? m)
  (<= (missile-y m) 0))
      

;; Invader ListOfMissile -> Boolean
;; returns true if given invader is within HIT-RANGE of any missiles in ListOfMissile
(check-expect (i-collision? I1 (list M1)) false)
(check-expect (i-collision? I1 (list M1 M2)) true)
(check-expect (i-collision? I1 (list M1 M3)) true)
(check-expect (i-collision? I11 (list M1 M2 M3)) false)

;(define (i-collision? i lom) false)  ;stub

;<used template from Invader>

(define (i-collision? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (>= (missile-x (first lom)) (- (invader-x i) HIT-RANGE))
                  (<= (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                  (<= (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
             true
             (i-collision? i (rest lom)))]))








;; Game KeyEvent -> Game
;; changes direction of tank when user presses left or right arrow key, fires missile when user presses space
(check-expect (handle-key G0 "\b") G0)
(check-expect (handle-key G0 "left") (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key G0 "right") (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key G3 " ") (make-game (list I1 I2) (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2)) M1 M2) T1))
(check-expect (handle-key G4 "left") G4)
(check-expect (handle-key G5 "right") G5)
                          

;(define (handle-key g ke) G0)  ;stub

;<used template from Game>

(define (handle-key g ke)
  (cond [(string=? ke "left") 
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(string=? ke "right")
        (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(string=? ke " ")
         (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g)) (game-tank g))]
        [else
         g]))
        
         
         






;; Game -> Image
;; renders image of current game containing invaders, missiles and a tank
(check-expect (render-game G0) (place-images
                                (list TANK)
                                (list (make-posn (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)))
                                BACKGROUND))
(check-expect (render-game G2) (place-images
                                (list TANK
                                      MISSILE
                                      INVADER)
                                (list (make-posn 50 (- HEIGHT TANK-HEIGHT/2))
                                      (make-posn 150 300)
                                      (make-posn 150 100))
                                BACKGROUND))
                                      

;(define (render-game g) BACKGROUND)  ;stub

;<used template from Game>

(define (render-game g)
  (place-images
   (append (invader-list (game-invaders g))
           (missile-list (game-missiles g))
           (list TANK))
   (append (invader-posn (game-invaders g))
           (missile-posn (game-missiles g))
           (tank-posn (game-tank g)))
   BACKGROUND))







;; ListOfInvader -> ListOfImage
;; produces a list of INVADER images
(check-expect (invader-list empty) empty)
(check-expect (invader-list (list I1 I2 I3)) (list INVADER INVADER INVADER))
                                                               
;(define (invader-list loi) (list BACKGROUND)  ;stub

;<used template from Invader>

(define (invader-list loi)
  (cond [(empty? loi) empty]
      [else
       (cons INVADER (invader-list (rest loi)))]))


;; ListOfMissile -> ListOfImage
;; produces a list of MISSILE images
(check-expect (missile-list empty) empty)
(check-expect (missile-list (list M1 M2 M3)) (list MISSILE MISSILE MISSILE))

;(define (missile-list lom) (list BACKGROUND)  ;stub

;<used template from Missile>

(define (missile-list lom)
  (cond [(empty? lom) empty]
      [else
       (cons MISSILE (missile-list (rest lom)))]))




;; ListOfInvader -> ListOf posn
;; produces a list of posn, where each posn describes the x and y coordinate of an Invader in ListOfInvader
(check-expect (invader-posn empty) empty)
(check-expect (invader-posn (list I1)) (list (make-posn 150 100)))
(check-expect (invader-posn (list I1 I10 I11)) (list (make-posn 150 100)
                                                     (make-posn 150 300)
                                                     (make-posn 0 0)))

;(define (invader-posn loi) empty)  ;stub

;<used template from Invader>

(define (invader-posn loi)
   (cond [(empty? loi) empty]
      [else
       (cons (make-posn (invader-x (first loi)) (invader-y (first loi)))
             (invader-posn (rest loi)))]))


;; ListOfMissile -> ListOf posn
;; produces a list of posn, where each posn describs the x and y coordinates of a Missile in ListOfMissile
(check-expect (missile-posn empty) empty)
(check-expect (missile-posn (list M1)) (list (make-posn 150 300)))
(check-expect (missile-posn (list M1 M5 M6)) (list (make-posn 150 300)
                                                   (make-posn 50 400)
                                                   (make-posn 200 50)))

;(define (missile-posn lom) empty)  ;stub

;<used template from Missile>

(define (missile-posn lom)
  (cond [(empty? lom) empty]
      [else
       (cons (make-posn (missile-x (first lom)) (missile-y (first lom)))
             (missile-posn (rest lom)))]))




;; Tank -> ListOf posn
;; produces a list of a single posn which describes the x and y coordinates of the Tank
(check-expect (tank-posn T0) (list (make-posn (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2))))
(check-expect (tank-posn T1) (list (make-posn 50 (- HEIGHT TANK-HEIGHT/2))))

;(define (tank-posn t) empty)  ;stub

;<used template from Tank>

(define (tank-posn t)
  (list (make-posn (tank-x t) (- HEIGHT TANK-HEIGHT/2))))
            








;; Game -> Boolean
;; produces true when an invader reaches the bottom of the screen, which ends the world program
(check-expect (game-over? G0) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)
(check-expect (game-over? G6) true)

;(define (game-over? g) true)  ;stub

;<used template from Game>

(define (game-over? g)
  (reached-bottom? (game-invaders g)))




;; ListOfInvader -> Boolean
;; produces true if any Invader in ListOfInvader has reached the bottom of the screen
(check-expect (reached-bottom? empty) false)
(check-expect (reached-bottom? (list I1 I2 I3)) true)
(check-expect (reached-bottom? (list I3)) true)
(check-expect (reached-bottom? (list I12)) true)

;(define (reached-bottom? loi) false)  ;stub

;<used template from Invader>
  
(define (reached-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) (- HEIGHT (/ (image-height INVADER) 2)))
             true
             (reached-bottom? (rest loi)))]))
         

       
       