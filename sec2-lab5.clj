(def thru
  (fn [etc start end]
    (loop
      [index start]
      (if
        (< index end)
        (do
          (etc index)
          (recur (+ index 1)))))))

(def choosing
  (fn [etc n k e]
    (if
      (= etc println)
      (do
        (etc (choosing conj n k e))
        (if
          (= e 0)
          (thru
            (fn [j]
              (choosing etc n k j))
            (+ e 1) (+ 2 (- n k)))))
      (if
        (< 0 k)
        (thru
          (fn [])

(def choose
  (fn [etc n k]
    (choosing etc n k 0)))

(choosing println 5 3 0)
