(ns basicone.sesssix.two)

;; euler 2 => sum even fibo < 4000000

(defn fibo1
  [n]
  (cond (== n 0) 1
        (== n 1) 2
        :else (+ (fibo1 (- n 1))
                 (fibo1 (- n 2)))))

(defn sol2a
  [lim]
  (->> (range)
       (map fibo1)
       (take-while #(< % lim))
       (filter even?)
       (reduce +)))

(defn fibo2
  [n]
  (->> [1 2]
       (iterate #(let [[a b] %] [b (+ a b)]))
       (map first)
       (take n)))

(defn sol2b
  [lim]
  (->> (fibo2 100)
       (take-while #(< % lim))
       (filter even?)
       (reduce +)))

(def fibo3 (->> [1 2]
                (iterate #(let [[a b] %] [b (+ a b)]))
                (map first)))

(def power-of (->> (range)
                   (map (fn [x] (iterate #(* % x) 1)))
                   (partial nth)))

(def fibos (conj (lazy-seq (map + fibos (rest fibos))) 2 1))



















