;CSci 2041
;Computer Laboratory 3
;section 2
;Akifumi Fujita 5286062

(def mergesort
  (fn [less? unsorted]
    (declare splitting)
    (declare merging)
    (letfn
      [(splitting [unsorted left right]
        (if
          (empty? unsorted)
          (merging (mergesort less? left) (mergesort less? right))
          (if
            (= '() (rest unsorted))
            (merging
              (mergesort less? (cons (first unsorted) left))
              (mergesort less? right))
            (splitting
              (rest (rest unsorted))
              (cons (first unsorted) left)
              (cons (second unsorted) right)))))
      (merging [left right]
        (if
          (empty? left)
          right
          (if
            (empty? right)
            left
            (if
              (less? (first left) (first right))
              (cons (first left) (merging (rest left) right))
              (cons (first right) (merging left (rest right)))))))]
    (if
      (or
        (empty? unsorted)
        (= '() (rest unsorted)))
      unsorted
      (splitting unsorted '() '())))))

(mergesort < '(9 8 7 6 5))  ;=> (5 6 7 8 9)
