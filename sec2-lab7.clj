(def map2
  (fn [function elements]
    (if
      (empty? elements)
      '()
      (concat
        (function (first elements) (second elements))
        (map2 function (rest (rest elements)))))))

(defmacro which [S & forms]
  (let [g (gensym)]
    (list 'let
      (vector g S)
      (cons 'cond
      (map2
          (fn [L F]
            (list (list '= g L) F))
          forms)))))

(defmacro most
  ([] true)
  ([& forms]
    (cons '>
      (list (cons '+
        (map
          (fn [form]
            (list 'if form 1 0))
            forms))
      2))))

(def comps-helper
  (fn [g k]
    (if
      (= k 1)
      (list g 'x)
      (cons g (list (comps-helper g (- k 1)))))))

(defmacro comps [f k]
  (if
    (= k 0)
    (list 'fn '[x] 'x)
    (if
      (= k 1)
      f
      (let [g (gensym)]
        (list 'let
          (vector g f)
          (list 'fn '[x] (comps-helper g k)))))))

(defmacro qrat [s a b c]
  `(/
    (~s
      (- ~b
      (Math/sqrt
        (-
          (* ~b ~b)
          (* 4 ~a ~c)))))
    (* 2 ~a)))
