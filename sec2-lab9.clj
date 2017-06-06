;CSci 2041
;Computer Laboratory 9
;section 2
;Akifumi Fujita 5286062

(def exp
  (fn [x n]
    (if
      (= 0 n)
      1
      (* x (exp x (dec n))))))

(def factorial
  (fn [n]
    (if
      (= 0 n)
      1
      (* n (factorial (dec n))))))

(def make-list
  (fn [x n]
    (list
      (/ (exp x n) (factorial n))
      (fn []
        (make-list x (inc n))))))

(def euler
  (fn [x]
    (make-list x 0)))

(println (euler 1.0))

(def next-term
  (fn [terms]
    (first terms)))

(println (next-term (euler 1.0))) ;=>1

(def remaining-terms
  (fn [terms]
    ((first (rest terms)))))

(println (next-term (remaining-terms (euler 1.0))));=>1.0

(def sum
  (fn [terms epsilon]
    (if
      (< (next-term terms) epsilon)
      (next-term terms)
      (+ (next-term terms) (sum (remaining-terms terms) epsilon)))))

(sum (euler 1.0) 0.000001) ;=>2.7182818011463845
