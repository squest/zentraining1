;Do euler no 1, 2, 6, 16, 20, 25


(defn fibo-sum [first second range]
  (if (>= range first)
      (+ first (fibo-sum second (+ first second) range))
      0))


;(defn round? [x]
;  (cond (> x 0) (round? (- x 1))
;    (= x 0) true
;    :else false))


;(defn is-even [x]
;  (if (= (round? x) true)
;    (round? (/ x 2))
;    false))
        

;(defn fibo-even-sum [first second range]
;  (if (>= range first)
;      (if (is-even first)
;          (+ first (fibo-even-sum second (+ first second) range))
;          (fibo-even-sum second (+ first second) range))
;     0))