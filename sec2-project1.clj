;CSci 2041
;Programming Project1
;section 2
;Akifumi Fujita 5286062

(def op
  (fn [list]
    (first list)))
(def left
  (fn [list]
    (first (rest list))))
(def right
  (fn [list]
    (second (rest list))))
(def if?
  (fn [list]
    (if
      (seq? list)
      (= 'if (first list))
      false)))
(def make-if
  (fn [a b c]
    (list 'if a b c)))
(def if-test
  (fn [list]
    (second list)))
(def if-then
  (fn [list]
    (second (rest list))))
(def if-else
  (fn [list]
    (second (rest (rest list)))))

(def ifify
  (fn [P]
    (if
      (symbol? P)
        P
        (if
          (= 'not (op P))
          (make-if (ifify (left P)) false true)
          (cond
            (= 'and (op P)) (make-if (ifify (left P)) (ifify (right P)) false)
            (= 'or (op P)) (make-if (ifify (left P)) true (ifify (right P)))
            (= 'imply (op P)) (make-if (ifify (left P)) (ifify (right P)) true)
            (= 'equiv (op P))
            (make-if (ifify (left P)) (ifify (right P))
              (make-if (ifify (right P)) false true)))))))

;(println (ifify '(imply (not a) (and b c))))  ;=>(if (if a false true) (if b c false) true)
;(println (ifify '(equiv (or a b) d)))  ;=>(if (if a true b) d (if d false true))

(def normalize
  (fn [C]
    (if
      (if? C)
        (if
          (if? (if-test C))
          (normalize (make-if (normalize (if-test (if-test C)))
            (normalize (make-if (if-then (if-test C))
              (normalize (if-then C))
              (normalize (if-else C))))
            (normalize (make-if (if-else (if-test C))
              (normalize (if-then C))
              (normalize (if-else C))))))
          C)
      C)))

;(println (normalize '(if (if (= 1 3) a b) c d))) ;=>(if (= 1 3) (if a c d) (if b c d))

(def substitute
  (fn [C V B]
    (if
      (seq? C)
        (if
          (empty? C)
            '()
            (if
              (= V (first C))
              (cons B (substitute (rest C) V B))
              (cons (first C) (substitute (rest C) V B))))
      C)))

;(println (substitute '(if a b c) 'b true)) ;=>(if a true c)


(def simplify
  (fn [C]
    (if
      (if? C)
      (if
        (= true (simplify (if-test C)))
        (simplify (if-then C))
        (if
          (= false (simplify (if-test C)))
          (simplify (if-else C))
          (if
            (and (= true (simplify (if-then C))) (= false (simplify (if-else C))))
            (simplify (if-test C))
            (if
              (= (simplify (if-then C)) (simplify (if-else C)))
              (simplify (if-then C))
              (make-if (simplify (if-test C))
                (simplify (substitute (simplify (if-then C)) (simplify (if-test C)) true))
                (simplify (substitute (simplify (if-else C)) (simplify (if-test C)) false)))))))
      C)))

(def tautology?
  (fn [P]
    (if
      (simplify (normalize (ifify P)))
      true
      false)))

(println (tautology? '(imply (not (and p q)) (or (not p) (not q)))))  ;=>true
