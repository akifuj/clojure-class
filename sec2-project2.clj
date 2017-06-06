;CSci 2041
;Computer Project 2
;section 2
;Akifumi Fujita 5286062

(def evaluate)

(def scope
 (list
  (assoc
   (hash-map)
   '+     +
   '-     -
   '*     *
   '/     /
   '=     =
   '<     <
   'true  true
   'false false)))

(def closure?
 (fn [object]
  (and
   (vector? object)
   (= (get object 0) 'closure))))

(def closure-body
 (fn [closure]
  (get closure 2)))

(def closure-parameters
 (fn [closure]
  (get closure 1)))

(def constant?
 (fn [form]
  (or
   (nil? form)
   (number? form)
   (string? form)
   (= form true)
   (= form false))))

(def fn-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'fn))))

(def evaluate-call
 (fn [closure arguments scope]
  (loop
   [parameters (closure-parameters closure)
    arguments  arguments
    map        (hash-map)]
   (if
    (empty? parameters)
    (evaluate
     (closure-body closure)
     (cons map
      (closure-scope closure)))
    (recur
     (rest parameters)
     (rest arguments)
     (assoc map
      (first parameters)
      (evaluate (first arguments) scope)))))))

(def evaluate-if
 (fn [form scope]
  (if
   (evaluate (first (rest form)) scope)
   (evaluate (first (rest (rest form))) scope)
   (evaluate (first (rest (rest (rest form)))) scope))))

(def evaluate-let
 (fn [form scope]
  (loop
   [pairs (second form)
    map   (hash-map)]
   (if
    (empty? pairs)
    (evaluate
     (first (rest (rest form)))
     (cons map scope))
    (recur
     (rest (rest pairs))
     (assoc map
      (first pairs)
      (evaluate
       (second pairs)
       (cons map scope))))))))

(def evaluate-symbol
 (fn
  ([symbol]
   (evaluate-symbol symbol scope))

  ([symbol scope]
   (if
    (empty? scope)
    (throw (Exception. (str "Unbound symbol: " symbol)))
    (if
     (contains? (first scope) symbol)
     (get (first scope) symbol)
     (recur symbol (rest scope)))))))

(def if-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'if))))

(def let-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'let))))

(def quote-call?
 (fn [form]
  (and
   (not (empty? form))
   (= (first form) 'quote))))

(def letfn-call?
 (fn [form]
   (and
     (not (empty? form))
     (= (first form) 'letfn))))

(def evaluate-letfn
 (fn [form scope]
   (loop
     [pairs (second form)
       map (hash-map)]
     (if
       (empty? pairs)
       (evaluate
         (last form)
         (cons map scope))
       (recur
         (rest (rest pairs))
         (assoc map
           (first pairs)
           (evaluate-fn
             (second pairs)
             (cons map scope))))))))

(def evaluate-fn
 (fn [form scope]
     (vector 'closure
       (second form)
       (first (rest (rest form)))
       (atom scope))))

(def closure-scope
 (fn [closure]
   (deref (get closure 3))))

(def evaluate
 (fn
  ([form]
   (evaluate form scope))
  ([form scope]
   (cond
    (constant? form)
    form
    (symbol? form)
    (evaluate-symbol form scope)
    (fn-call? form)
    (evaluate-fn form scope)
    (if-call? form)
    (evaluate-if form scope)
    (let-call? form)
    (evaluate-let form scope)
    (letfn-call? form)
    (evaluate-letfn form scope)
    (quote-call? form)
    (second form)
    true
    (let
     [function (evaluate (first form) scope)]
     (if
      (closure? function)
      (evaluate-call function (rest form) scope)
      (apply function
       (map
        (fn [argument]
         (evaluate argument scope))
        (rest form)))))))))

(evaluate
  '(letfn
    (factorial
      (fn (n)
        (if
          (= n 0)
          1
          (* n (factorial (- n 1))))))
    (factorial 5)))
