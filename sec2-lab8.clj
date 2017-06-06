(declare lamda? lamda-fn)

(def member?
  (fn [E S]
    (some
      (fn [e]
        (= E e))
        S)))

(def lamda-symbol?
  (fn [N S]
    (if
      (symbol? N)
      (member? N S)
      (if
        (seq? N)
        (if
          (empty? N)
          true
          (if
            (= 'fn (first N))
            (lamda? N S)
            (and (lamda-symbol? (first N) S)
              (lamda-symbol? (rest N) S))))))))

(def unique?
  (fn [P]
    (if
      (empty? (rest P))
      true
      (and
        (not (member? (first P) (rest P)))
        (unique? (rest P))))))

(def lamda-parameteres?
  (fn [P]
    (if
      (vector? P)
      (unique? P))))

(def lamda-fn?
  (fn [L S]
    (and
      (= 'fn (first L))
      (lamda-parameteres? (second L)))))

(def lamda-call?
  (fn [L S]
    (if
      (seq? L)
      (= 3 (count L)))))

(def vectoring
  (fn [S V]
    (if
      (empty? V)
      S
      (conj (vectoring S (rest V)) (first V)))))

(def lamding
  (fn [L S]
    (if
      (and (lamda-call? L S) (lamda-fn? L S))
      (lamda-symbol?
        (second (rest L))
        (vectoring S (second L))))))

(defn lamda?
  ([L] (lamding L []))
  ([L S] (lamding L S)))

(println (lamda? '(fn [a] a)))  ;=>true
(println (lamda? '(fn [a] (fn [] a))))  ;=>true
(println (lamda? '(fn [a b] b)))  ;=>true
(println (lamda? '(fn [a b c] (a (fn [d e] (a b)) c)))) ;=>true
(println (lamda? '(a b))) ;=>nil
(println (lamda? '(fn [a b] ((fn [c] (a b)) c)))) ;=>nil
