;CSci 2041
;Computer Laboratory 1
;Akifumi FUjita 5286062

;exponential
(def epsilon 0.000001)

(def f 1.0)
(def s 0.0)
(def t 1.0)

(def calculation
  (fn [f s t x]
  (if
    (<= t epsilon)
      s
      (calculation
        (+ f 1.0)
        (+ s t)
        (* t ( / x f)) x ))))

(def exponential
  (fn [x]
    (calculation f s t x)))

(println (exponential 2.0))

;square-root
(def h 1.0)

(def abs
  (fn [n]
    (if
      (< n 0)
        (* n -1)
        n)))

(def calculation2
  (fn [g h x]
  (if
    (< (abs (- g h)) epsilon)
      g
      (calculation2
        (/ (+ g h) 2.0)
        (/ x g) x ))))

(def square-root
  (fn [x]
    (calculation2 x h x)))

(println(square-root 2.0))
