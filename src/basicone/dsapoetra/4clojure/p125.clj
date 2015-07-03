(ns basicone.dsapoetra.4clojure.p125)

;;107
(fn [x]
  (fn [y] (reduce * (take x (repeat y)))))

;;118
(fn map1 [f col]
  (if (seq col) (lazy-seq
                  (cons (f (first col)) (map1 f (rest col))))
                nil))

;;120
(fn cnt-sqrt [arg]
  (let [get-digits (fn [n]
                     (map #(Integer/valueOf (str %)) (String/valueOf n)))
        digits-sqr (fn [n]
                     (apply + (map #(* % %) (get-digits n))))
        res-seq (filter #(< % (digits-sqr %)) arg)]
    (count res-seq)))

;;122
(fn [s]
  (Integer/parseInt s 2))

(fn [strings]
  (let [coll (seq (clojure.string/split strings #","))]
    (filter #(= 3 (count (str (Math/sqrt (int %))))) coll)))

(fn [number]
  (let [dig (fn explode-to-digits [number]
              (if (= number 0)
                []
                (let [d (rem number 10)
                      r (quot number 10)]
                  (conj (explode-to-digits r) d))))
        newnum (reduce + (map #(* % %) (seq (dig number))))]
    (cond
      (= 1 newnum) true
      (= 4 newnum) false)
      :else (recur (newnum))))

(fn explode-to-digits [n]
  (if (= n 0)
    []
    (let [d (rem n 10)
          r (quot n 10)]
      (conj (explode-to-digits r) d))))

(defn coba [x]
  (let [dig (fn explode-to-digits [x]
              (if (= x 0) []
                (let [d (rem x 10)
                      r (quot x 10)]
                  (conj (explode-to-digits r) d))))]
    (cond
      (= 1 (reduce + (map #(* % %) (seq (dig x))))) true
      (= 4 (reduce + (map #(* % %) (seq (dig x))))) false
      :else (recur (reduce + (map #(* % %) (seq (dig x))))))))

(defn palind [x]
  (if (= (str x) (apply str (reverse (str x))))
         true
         false))