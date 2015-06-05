(ns basicone.dsapoetra.euler.p25)

;;util
(defn factors [n]
  (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn prime? [n]
  (= 2 (reduce +
               (for [i (range 1 (inc n))]
                 (if (= 0 (mod n i)) 1 0)))))


(defn palindrome? [n]
  (= (->> n str reverse (apply str))
     (str n)))

(defn fib [a b] (cons a (lazy-seq (fib b (+' b a)))))

(defn power [x y]
  (if (= y 1)
    x (*' x (power x (dec y)))))

;;1
(defn kelipatan []
  (reduce + (distinct (concat (range 3 1000 3) (range 5 1000 5)))))

;;2
(defn fibfourmillion [x]
  (if (> 4000000 (apply max (take x (fib 1 2))))
    (recur (+ 1 x))
    (reduce + (filter even? (take x (fib 1 2))))))

;;3
(defn faktor-prima [x]
  (apply max (filter prime? (factors x))))



;;4
(defn largest-palindrome []
  (apply max (filter #(palindrome? %)
                     (for [x (range 100 (inc Integer/MAX_VALUE))
                           y (range 100 (inc Integer/MAX_VALUE))]
                       (* x y)))))

;;5

(defn divisible [x]
  (= 0 (rem x 20) (rem x 19) (rem x 18) (rem x 17) (rem x 16) (rem x 15) (rem x 14) (rem x 13) (rem x 12) (rem x 11) (rem x 10) (rem x 9) (rem x 8) (rem x 7) (rem x 6) (rem x 5) (rem x 4) (rem x 3) (rem x 2) (rem x 1)))

(defn small [x y]
  (if (< x y)
    (if (divisible x)
      x
      (recur (+ x 1) y))
    "tetot rangenya salah"))

;;6

(defn margin-natural [x]
  (- (power (reduce + (range 1 (+ x 1))) 2) (reduce + (map #(power % 2) (range 1 (+ x 1))))))

;;7
(defn first-n-prime [x]
  (take x (filter prime? (range 1 Integer/MAX_VALUE))))