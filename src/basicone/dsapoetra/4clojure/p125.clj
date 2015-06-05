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

