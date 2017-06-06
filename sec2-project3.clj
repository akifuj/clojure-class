;CSci 2041
;Computer Project 2
;section 3
;Akifumi Fujita 5286062

(def error)
(def pairwise)
(def proc-parameters)
(def proc-result)
(def proc-subtype?)
(def proc-type?)
(def simple-subtype?)
(def simple-type?)
(def subtype?)

(def has-type?
  (fn [e t m]
    (if
      (symbol? e)
      (if
        (= t (get m e))
        true
        (subtype? t (get m e)))
      (and
        (every? proc-type? (get m (first e)))
        (some
          (fn [expression]
            (and
              (= (dec (count e)) (count (second expression)))
              (pairwise
                (fn [sub-a sub-t]
                  (has-type? sub-a sub-t m))
                (rest e)
                (second expression))
              (or
                (= t (last expression))
                (subtype? t (last expression)))))
          (get m (first e)))))))

(println
  (has-type?
    '(cons (first x) (rest x))
    'list
    {'x     '(list)
    'cons  '((proc (obj list) list))
    'first '((proc (list) obj))
    'rest  '((proc (list) list))}))

(def simple-subtypes
 (hash-map
   'bool  #{'int 'real 'complex}
   'int   #{'real 'complex}
   'real  #{'complex}))

(def subtype?
 (fn [left-type right-type]
  (cond
   (= left-type right-type)
   true
   (= right-type 'obj)
   true
   (simple-type? left-type)
   (simple-subtype? left-type right-type)
   (proc-type? left-type)
   (proc-subtype? left-type right-type)
   true
   (error "Type expected."))))

(def simple-type? symbol?)

(def simple-subtype?
 (fn [left-type right-type]
  (and
   (simple-type? right-type)
   (contains? (get simple-subtypes left-type) right-type))))

(def list-type?
 (fn [type]
  (and
   (not (empty? type))
   (= (first type) 'list))))

(def proc-type?
 (fn [type]
  (and
   (not (empty? type))
   (= (first type) 'proc))))

(def proc-subtype?
 (fn [left-type right-type]
  (and
   (proc-type? right-type)
   (subtype?
    (proc-result left-type)
    (proc-result right-type))
   (pairwise
    (fn [left-parameter right-parameter]
     (subtype? right-parameter left-parameter))
    (proc-parameters left-type)
    (proc-parameters right-type)))))

(def proc-result (comp first rest rest))

(def proc-parameters (comp first rest))

(def pairwise
 (fn [test? lefts rights]
  (if
   (or
    (empty? lefts)
    (empty? rights))
   (and
    (empty? lefts)
    (empty? rights))
   (and
    (test?
     (first lefts)
     (first rights))
    (recur test?
     (rest lefts)
     (rest rights))))))

(def error
 (fn [message]
  (throw (Exception. message))))
