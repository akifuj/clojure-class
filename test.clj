;CSci 2041
;Computer Laboratory 11
;section 2
;Akifumi Fujita 5286062

(def partial-ordering
  (hash-map
    'A  #{'C}
    'B  #{'H}
    'C  #{'G}
    'G  #{'D 'E}
    'D  #{'F}
    'E  #{'H}
    'H  #{'F}
    'I  #{'B 'E}))

(def precedes?
  (fn [left right]
    (contains? (get partial-ordering left) right)))

;(println(precedes? 'A 'C)) ;=> true

(def unpreceded?
  (fn [precedes? object other-objects]
    (not (some (fn [x] (precedes? x object)) other-objects))))

;(println(unpreceded? precedes? 'A '(A C G D I)))  ;=> false

(def delete
  (fn [objects object]
    (remove (partial = object) objects)))

;(println(delete '(B A N A N A) 'A)) ;=>(B N N)

(def unpreceding
  (fn [etc precedes? objects num]
    (if
      (= (count objects) num)
      nil
      (if
        (unpreceded? precedes? (nth objects num) objects)
        (nth objects num)
        (unpreceding etc precedes? objects (+ num 1))))))

(def unpreceded
  (fn [etc precedes? objects]
    (unpreceding etc precedes? objects 0)))

;(unpreceded println precedes? '(C G D A I)) ;=> A

(def satisfy
  (fn [precedes? objects]
    (if
      (empty? objects)
      nil
      (cons
        (unpreceded println precedes? objects)
        (satisfy precedes? (delete objects (unpreceded println precedes? objects)))))))

(satisfy precedes? '(A B C D E F G H I))
