;Do euler no 1, 2, 6, 16, 20, 25


;No 1
(defn multiple-max [x range]
  (if (< x range)
      (+ x (multiple-max x (- range x)))
      0))

(defn multiple-sum [x range]
  (if (> range x)
      (+ (multiple-max x range) (multiple-sum x (- range x)))
      0))

;(- (+ (multiple-sum 3 1000) (multiple-sum 5 1000)) (multiple-sum 15 1000))


;No 2
(defn fibo-sum [first second max-value]
  (if (>= max-value first)
      (+ first (fibo-sum second (+ first second) max-value))
      0))

(defn fibo-even-sum [first second max-value]
  (if (>= max-value first)
      (if (even? first)
          (+ first (fibo-even-sum second (+ first second) max-value))
          (fibo-even-sum second (+ first second) max-value))
     0))

;(defn round? [x]
;  (cond (> x 0) (round? (- x 1))
;    (= x 0) true
;    :else false))

;(defn is-even [x]
;  (if (= (round? x) true)
;    (round? (/ x 2))
;    false))


;No 6
(defn sum-of-squares [first end]
  (if (= first end)
    (square end)
    (+ (square first) (sum-of-squares (+ first 1) end))))

(defn square-of-sum [first end]
  (square (apply + (range first (+ end 1)))))


;No 16
(defn exp [a n]
  ;a^n
  (if (= n 1)
    a
    (*' a (exp a (-' n 1)))))

;;NO NEED TO USE THIS AT ALL;;
;(defn sum-of-exp [a n-begin n-end]
;  (if (= n-begin n-end)
;    (exp a n-end)
;    (+ (exp a n-begin) (exp a (+ n-begin 1)))))

(defn num-to-digits [n]
  (cond (= (quot n 10) 0) [n]
    :else (concat (num-to-digits (quot n 10)) [(mod n 10)])))
  
(defn sum-of-digits [n]
  (if (empty? n)
    0
    (+ (first n) (sum-of-digits (rest n)))))


;No 20
(defn factorial [x]
    (if (= x 1)
      1
      (*' x (factorial (- x 1)))))


;No 25
(defn list-fibo-maxval [first second max-value]
  (if (> second max-value)
    [first]
    (concat [first] (list-fibo-maxval second (+' first second) max-value))))


(defn max-fibo-digits [first second max-digits]
  (if (>= (count (num-to-digits first)) max-digits)
    (str "Max digits (user-defined): " max-digits 
         ", max value: " first 
         " with index: " (count (list-fibo-maxval 1 1 first)))
    (max-fibo-digits second (+' first second) max-digits)))