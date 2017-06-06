;CSci 2041
;Computer Laboratory 10
;section 2
;Akifumi Fujita 5286062

(def error
  (fn [message]
    (throw (Exception. message))))

(def Queue
  (fn []
    (letfn
      [(make-queue [top]
        (fn [method & arguments]
          (letfn
            [(empty? []
              (= top '()))
             (enqueue [e]
               (make-queue (cons e top)))
             (dequeue []
               (if (empty?)
                (error "Queue is empty.")
                (make-queue (butlast top))))
             (front []
               (if (empty?)
                (error "Queue is empty.")
                (first top)))]
            (cond
              (= method :empty?)
              (empty?)
              (= method :enqueue)
              (enqueue (first arguments))
              (= method :dequeue)
              (dequeue)
              (= method :front)
              (front)
              true
              (error "No such method.")))))]
      (make-queue '()))))

(def q0 (Queue))
(println (q0 :empty?))  ;=>true
(def q1 (q0 :enqueue "A"))
(println (q1 :empty?))  ;=>false
(println (q1 :front)) ;=>"A"
(def q2 (q1 :enqueue "B"))
(println (q2 :front)) ;=>"B"
(def q3 (q2 :dequeue))
(println (q3 :front)) ;=>"B"
