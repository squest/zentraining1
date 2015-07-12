(ns basicone.sessfour.problem-solving)

(defn expt
  [a m]
  (apply *' (repeat m a)))

(defn fibo-1
  [n]
  (cond (== 1 n) 1
        (== 2 n) 2
        :else (+ (fibo-1 (dec n))
                 (fibo-1 (->> n dec dec)))))

(defn fibo
  ([i]
   (if (= i 1) 1 (fibo i [2 1] 2)))
  ([i [a b] idx]
    (if (= idx i)
      a
      (recur i [(+' a b) a] (inc idx)))))

(defn lfibo-1
  [i]
  (->> [1 2]
       (iterate #(conj % (+' (last %) (last (butlast %)))))
       (take i)
       last))

(defn lfibo-2
  [i]
  (->> (iterate #(conj [(last %)] (apply +' %)) [0 1])
       (take i)
       (map last)))


(def lfibo-3
  (cons 1 (cons 1 (lazy-seq (map +' lfibo-3 (rest lfibo-3))))))

;;












































