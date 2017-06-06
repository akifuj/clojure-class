;CSci 2041
;Computer Laboratory 4
;section 2
;Akifumi Fujita 5286062

(def operator
  (fn [list]
    (first list)))
(def left-org
  (fn [list]
    (first (rest list))))
(def right-org
  (fn [list]
    (second (rest list))))

(def stackify
  (fn [calculation]
    (letfn
      [(pre-stackify [calculation]
        (if
          (not (seq? calculation))
          (cons 'push (list calculation))
          (if
            (= '() (rest calculation))
            (pre-stackify (first calculation))
            (if
              (nil? (right-org calculation))
              (concat
                (list (pre-stackify (left-org calculation)))
                '((neg)))
              (concat
                  (if
                    (and
                    (seq? (left-org calculation))
                    (seq? (right-org calculation)))
                    (concat
                      (pre-stackify (left-org calculation))
                      (pre-stackify (right-org calculation)))
                    (if
                      (or
                        (seq? (left-org calculation))
                        (seq? (right-org calculation)))
                      (if
                        (seq? (left-org calculation))
                        (concat
                          (pre-stackify (left-org calculation))
                          (list (cons 'push (list (right-org calculation)))))
                        (concat
                          (list (cons 'push (list (left-org calculation))))
                          (pre-stackify (right-org calculation))))
                      (concat
                        (list (cons 'push (list (left-org calculation))))
                        (list (cons 'push (list (right-org calculation)))))))
                (list (get {:+ '(add) :* '(mul) :/ '(div) :- '(sub)}
                  (keyword (operator calculation)))))))))]
    (concat (pre-stackify calculation) '((pop))))))

    (stackify '(- (* u v) w)) ;((push u) (push v) (mul) (push w) (sub) (pop))
