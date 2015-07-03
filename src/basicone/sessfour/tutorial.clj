(ns basicone.sessfour.tutorial)

(defn faktorial
  [n]
  (if (= n 1)
    1
    (*' n (faktorial (- n 1)))))

(defn faktorial-tail
  ([n] (faktorial-tail 1 1 n))
  ([i res n]
   (if (> i n)
     res
     (recur (+ i 1) (* res i) n))))

(defn faktorial-loop
  [n]
  (loop [i 1 res 1 m n]
    (if (> i n)
      res
      (recur (+ i 1) (* res i) m))))

(defn fibo
  [i]
  (cond (= i 1) 1
        (= i 2) 1
        :else (+ (fibo (- i 1))
                 (fibo (- i 2)))))

(defn fibo-tail
  ([i] (fibo-tail 1 1 i))
  ([a b i] (if (= i 1) a (recur (+ a b) a (- i 1)))))

(defn fibo-list
  ([i] (fibo-list 1 1 [1] i))
  ([a b res i] (if (= i 1)
                 (conj res a)
                 (recur (+ a b) a (conj res a) (- i 1)))))

(def fibo-gue
  (memoize
    (fn [n]
      (cond (= n 1) 1
            (= n 2) 1
            :else (+ (fibo-gue (- n 1))
                     (fibo-gue (- n 2)))))))

(defn qsort
  [[x & xs]]
  (if x
    (let [smaller (filter #(<= % x) xs)
          larger (filter #(> % x) xs)]
      (concat (qsort smaller) [x] (qsort larger)))
    []))

(defn max'
  [xs]
  (if (empty? xs)
    xs
    (let [counter (count xs)
          left (take (quot counter 2) xs)
          right (drop (quot counter 2) xs)
          max-left (max' left)
          max-right (max' right)]
      (if (> max-left max-right)
        max-left
        max-right))))

(defn palin?
  [xs]
  (= xs (reverse xs)))

(defn numcol
  [n]
  (if (< n 10) [n] (conj (numcol (quot n 10)) (rem n 10))))

(defn expt
  [a m]
  (if (== m 0) a (* a (expt a (dec m)))))
















