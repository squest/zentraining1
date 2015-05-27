(ns basicone.dsapoetra.zenleague.p25)

;;Utils

(defn explode-to-digits [number]
        (map #(- (int %) (int \0)) (str number)))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(defn fib [a b] (cons a (lazy-seq (fib b (+ b a)))))

(defn prime? [n]
   (= 2 (reduce +
                (for [i (range 1 (inc n))]
                  (if (= 0 (mod n i)) 1 0)))))

;; no 3



(defn decide-biggest-value [x y]
  (if (<= y x) (recur x (* y 6)) (if (> y x) (quot y 6 ) y)))

(defn base-six [x y]
 (if (< 1 y)
   (do
    (print (quot x y))
    (recur (rem x y) (quot y 6)))
   (quot x y)))

;; no 4

(defn one-p [x]
  (apply + (map factorial (explode-to-digits x))))

(defn n-p [x]
  (if (not= 0 x)
    (+ (one-p x) (n-p (- x 1)))
    x))

;;no 5

(defn fib-n [x]
  (apply max (take  x (fib 1 1))))

;; no 6

(defn check-position [x y z]
 (take z (range x 1000 y)))

;;no 7

(defn revenue [x]
  (if (not= 0 x)
      (+ (* 15000000 x) (revenue (- x 1))) x))

(defn expense [x]
  (if (not= 0 x)
      (+ (* 75000000 x) (expense (- x 1))) x))

(defn profit [x y]
  (- (revenue x) (expense y)))

;;no 7
(defn first-hundred []
  (take 100 (filter prime? (range 1 Integer/MAX_VALUE))))

(defn smallest-div [x]
  (let [y Integer/MAX_VALUE]
    (if (not= x y)
      (apply + (map #(/ y %) first-hundred))
      x)))

;;no 8
