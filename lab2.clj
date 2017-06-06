;CSci 2041
;Computer Laboratory 2
;Akifumi Fujita 5286062

(def nonempty?
  (fn [object]
    (and
      (list? object)
      (not (empty? object)))))

(def matching
  (fn [table pattern subject]
    (if (keyword? pattern)
      (if (= nil (get table pattern))
        (assoc table pattern subject)
        (if (= (get table pattern) subject)
        table
        nil))
      (if
        (and
          (seq? pattern)
          (seq? subject)
          (nonempty? pattern)
          (nonempty? subject))
        (if (= nil (matching table (first pattern) (first subject)))
          (matching table (rest pattern) (rest subject))
          (matching (matching table (first pattern) (first subject)) (rest pattern) (rest subject)))
        (if (= pattern subject)
          table
          nil)))))

(def match
  (fn [pattern subject]
    (matching (hash-map) pattern subject)))

(match '() '())
(match '() '(a b c))
(match '(:a is :a) '((a rose) is (a rose)))
(match '(cons (first :a) (rest :b)) '(cons (first x) (rest x)))
