#lang racket

;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x)
    (* x x)))

(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* 0.5 a (square t))
       (* v t)
       u)))

;; Problem 1. Test

(displayln "Problem 1. Test")

(= (position 0 0 0 0) 0)
(= (position 0 0 20 0) 20)
(= (position 0 5 10 10) 60)
(= (position 2 2 2 2) 10)
(= (position 5 5 5 5) 92.5)

;; Problem 2

(define discriminant
  (lambda (a b c)
    (- (square b)
       (* 4 a c))))

(define coeff-1
  (lambda (a b d)
    (/ (+ (* -1 b)
          (sqrt d))
       (* 2 a))))

(define coeff-2
  (lambda (a b d)
    (/ (- (* -1 b)
          (sqrt d))
       (* 2 a))))

(define root
  (lambda (a b c coeff)
    (let ((d (discriminant a b c)))
    (if (< d 0)
        '()
        (coeff a b d)))))

;; Problem 2. Test

(displayln "Problem 2. Test")

(= (root 2 -5 -3 coeff-1) 3)
(= (root 2 -5 -3 coeff-2) -0.5)
(= (root 1 -7 0 coeff-1) 7)
(= (root 1 -7 0 coeff-2) 0)
(equal? (root 1 1 1 coeff-1) '())

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root (* -0.5 gravity) vertical-velocity elevation coeff-2)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

;; Problem 3. Test

(displayln "Problem 3. Test")

(time-to-impact 10 0)
;; => 2.04
(time-to-impact 50 0)
;; => 10.2
(time-to-impact 100 0)
;; => 20.4
(time-to-impact 27 0)
;; => 5.51

(time-to-height 0 15 0)
;; => 1.74
(time-to-height 5 10 10)
;; => 1.02
(time-to-height 18 5 20)
;; => 2.39
(time-to-height 15 0 30)
;; '()

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure

(define degree2radian
  (lambda (deg)
    (/ (*  deg pi)
       180.)))

(define x-velocity
  (lambda (velocity rad-angle)
    (* velocity (cos rad-angle))))

(define y-velocity
  (lambda (velocity rad-angle)
    (* velocity (sin rad-angle))))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((rad-angle (degree2radian angle)))
      (let ((vx (x-velocity velocity rad-angle))
            (time (time-to-impact (y-velocity velocity rad-angle) elevation)))
        (position 0 vx 0 time)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

(displayln "Problem 4. Test")

(travel-distance-simple 1 45 0)
;; => 20.328928
(travel-distance-simple 1 45 90)
;; => 5.496e-4 
(travel-distance-simple 1 45 45)
;; => 207.6278611

(meters-to-feet (travel-distance-simple 1 45 0))
;; => 67.08546179
(meters-to-feet (travel-distance-simple 1 45 45))
;; => 685.1719418
(meters-to-feet (travel-distance-simple 1 45 90))
;; => 1.8138183e-3


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.5)

(define find-best-angle-iter
  (lambda (elevation velocity start-angle end-angle)
    (if (= start-angle end-angle)
        end-angle
        (choose-best-angle elevation velocity (find-best-angle-iter elevation velocity start-angle (- end-angle alpha-increment)) end-angle))))

(define choose-best-angle
  (lambda (elevation velocity a b)
    (if (> (travel-distance-simple elevation velocity a) 
           (travel-distance-simple elevation velocity b))
           a b)))

(define find-best-angle
  (lambda (velocity elevation)
    (find-best-angle-iter velocity elevation 0 90)))

;; find best angle
;; try for other velocities
;; try for other heights

(displayln "Problem 5. Test")

(find-best-angle 0 25)
;; => 45.0
(find-best-angle 0 50)
;; => 45.0
(find-best-angle 10 25)
;; => 41.0
(find-best-angle 10 50)
;; => 44.0

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* pi .25 (square diameter))))

(define dx
  (lambda (u dt)
    (* u dt)))

(define dy
  (lambda (v dt)
    (* v dt)))

(define du
  (lambda (u v dt m beta)
    (- (/ (* beta
             (sqrt (+ (square u)
                      (square v)))
             u
             dt)
          m))))

(define dv
  (lambda (u v dt m beta g)
    (- (* (+ (/ (* beta
                   (sqrt (+ (square u)
                            (square v)))
                   v)
                m)
             g)
          dt))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0)
        x0
        (integrate
         (+ x0 (dx u0 dt))
         (+ y0 (dy v0 dt))
         (+ u0 (du u0 v0 dt m beta))
         (+ v0 (dv u0 v0 dt m beta g))
         dt
         g
         m
         beta))))

(define travel-distance
  (lambda (elevation velocity angle)
    (let ((rad-angle (degree2radian angle)))
      (integrate
       0
       elevation
       (x-velocity velocity rad-angle)
       (y-velocity velocity rad-angle)
       0.1
       gravity
       mass
       beta))))

(displayln "Problem 6. Test")

(meters-to-feet (travel-distance 0 45 45))
;; => 305.27
(meters-to-feet (travel-distance 0 40 45))
;; => 269.6
(meters-to-feet (travel-distance 0 35 45))
;; => 232.62

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define integrate-time
  (lambda (x0 y0 u0 v0 t0 dt g m beta)
    (if (< y0 0)
        t0
        (integrate-time
         (+ x0 (dx u0 dt))
         (+ y0 (dy v0 dt))
         (+ u0 (du u0 v0 dt m beta))
         (+ v0 (dv u0 v0 dt m beta g))
         (+ t0 dt)
         dt
         g
         m
         beta))))

(define travel-time
  (lambda (elevation velocity angle)
    (let ((rad-angle (degree2radian angle)))
      (integrate-time
       0
       elevation
       (x-velocity velocity rad-angle)
       (y-velocity velocity rad-angle)
       0
       0.1
       gravity
       mass
       beta))))

(define hit?
  (lambda (traveled-distance target-distance tolerance)
    (if (< (abs (- traveled-distance
                   target-distance))
           tolerance)
        #t
        #f)))

(define throw-ball-iter
  (lambda (elevation velocity target-distance tolerance start-angle end-angle shortest-time)

    (define next-iter
      (lambda (time)
        (throw-ball-iter elevation velocity target-distance tolerance (+ start-angle alpha-increment) end-angle time)))
  
    (if (> start-angle end-angle)
        shortest-time
        (let ((traveled-distance (travel-distance elevation velocity start-angle)))
          (if (hit? traveled-distance target-distance tolerance)
              (let ((t0 (time-to-impact (y-velocity velocity (degree2radian start-angle)) elevation)))
                (cond ((or (= shortest-time 0) (< t0 shortest-time))
                       (next-iter t0))
                      (else
                       (next-iter shortest-time))))
              (next-iter shortest-time))))))

(define throw-ball
  (lambda (elevation velocity desired-distance)
    (throw-ball-iter elevation velocity desired-distance 3 -90 90 0)))

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

(displayln "Problem 7. Test")

(throw-ball 1 55 36)
;; => 0.56
(throw-ball 1 45 36)
;; => 0.63
(throw-ball 1 35 36)
;; => 0.95

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

(throw-ball 1 45 30)
;; => 0.53
(throw-ball 1 45 60)
;; => 1.64
(throw-ball 1 45 90)
;; => 3.93

;; Problem 8

(define travel-distance-bounce-iter
  (lambda (elevation velocity angle bounces bounce-count total-distance)
    (let ((current-distance (+ total-distance (travel-distance elevation velocity angle))))
      (if (= bounces bounce-count)
          current-distance
          (let ((traveled-distance (travel-distance elevation velocity angle)))
            (travel-distance-bounce-iter 0 (/ velocity 2) angle bounces (+ bounce-count 1) current-distance))))))

(define travel-distance-bounce
  (lambda (elevation velocity angle bounces)
    (travel-distance-bounce-iter elevation velocity angle bounces 0 0)))

(define travel-distance-bounce-inf-iter
  (lambda (elevation velocity angle total-distance)
    (let ((current-distance (+ total-distance (travel-distance elevation velocity angle))))
      (if (< velocity 0.01)
          current-distance
          (let ((traveled-distance (travel-distance elevation velocity angle)))
            (travel-distance-bounce-inf-iter 0 (/ velocity 2) angle current-distance))))))

(define travel-distance-inf-bounce
  (lambda (elevation velocity angle)
    (travel-distance-bounce-inf-iter elevation velocity angle 0)))


(displayln "Problem 8. Test")

(travel-distance-bounce 1 45 45 0)
;; => 92.5
(travel-distance-bounce 1 45 45 1)
;; => 132.12
(travel-distance-bounce 1 45 45 2)
;; => 144.79

(travel-distance-inf-bounce 1 45 45)
;; => 150.52

;; Problem 9

(define integrate-velocity
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0)
        (sqrt (+ (square u0) (square v0)))
        (integrate-velocity
         (+ x0 (dx u0 dt))
         (+ y0 (dy v0 dt))
         (+ u0 (du u0 v0 dt m beta))
         (+ v0 (dv u0 v0 dt m beta g))
         dt
         g
         m
         beta))))

(define travel-velocity
  (lambda (elevation velocity angle)
    (let ((rad-angle (degree2radian angle)))
      (integrate-velocity
       0
       elevation
       (x-velocity velocity rad-angle)
       (y-velocity velocity rad-angle)
       0.1
       gravity
       mass
       beta))))

(define travel-distance-bounce-integrate-iter
  (lambda (elevation velocity angle bounces bounce-count total-distance)
    (let ((current-distance (+ total-distance (travel-distance elevation velocity angle))))
      (if (= bounces bounce-count)
          current-distance
          (let ((traveled-velocity (travel-velocity elevation velocity angle)))
            (travel-distance-bounce-integrate-iter 0 (/ traveled-velocity 2) angle bounces (+ bounce-count 1) current-distance))))))

(define travel-distance-integrate-bounce
  (lambda (elevation velocity angle bounces)
    (travel-distance-bounce-integrate-iter elevation velocity angle bounces 0 0)))

(displayln "Problem 9. Test")

(travel-distance-integrate-bounce 1 45 45 0)
;; => 92.5
(travel-distance-integrate-bounce 1 45 45 1)
;; => 106.75
(travel-distance-integrate-bounce 1 45 45 2)
;; => 110.79