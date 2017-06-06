(def take-num
  (fn [S num]
    (if
      (= num 0)
      '()
      (cons (nth S (- num 1)) (take-num S (- num 1))))))

(def unique-filter
  (fn [F S num]
    (if
      (some
        (fn [e]
          (= e (nth S num)))
          (take-num S num))
      nil
      (F (nth S num)))))

(def unique
  (fn [F S]
    (loop
      [n 0]
      (if
        (= n (count S))
        nil
        (do
          (unique-filter F S n)
          (recur (+ n 1)))))))

(unique println '(a b a c b c c a b))
;=>a
;  b
;  c


(def make-sets
  (fn [F U V u v]
    (if
      (and (= (+ u 1) (count U)) (= (+ v 1) (count V)))
      nil
      (if
        (= (+ v 1) (count V))
        (concat (list (F (nth  U (+ u 1)) (nth V 0))) (make-sets F U V (+ u 1) 0))
        (concat (list (F (nth U u) (nth V (+ v 1)))) (make-sets F U V u (+ v 1)))))))

(def cartesian
  (fn [F U V]
    (make-sets F U V 0 -1)))

(println (cartesian + '(3 4 1) '(4 2))) ;=>(7 5 8 6 5 3)

(def nest
  (fn [F N & A]
    (fn [& A]
      (if
        (= N 0)
        A
        (comp F (nest F (- N 1) A))))))
