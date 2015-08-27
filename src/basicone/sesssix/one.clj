(ns basicone.sesssix.one)

;; using euler 1

(defn sol1a
  [lim]
  (- (+ (reduce + (range 3 lim 3))
        (reduce + (range 5 lim 5)))
     (reduce + (range 15 lim 15))))

(defn sol1b
  [lim]
  (->> (range 5 lim 5)
       (concat (range 3 lim 3))
       distinct
       (reduce +)))

(defn sol1c
  [lim]
  (->> (range 1 lim)
       (filter #(or (zero? (rem % 3))
                    (zero? (rem % 5))))
       (reduce +)))

(defn sol1d
  [lim]
  (transduce
    (filter #(or (zero? (rem % 3))
                 (zero? (rem % 5))))
    + (range 1 lim)))

(defn ^long sol1e
  [^long lim]
  (loop [i (int 3) res (int 0)]
    (if (>= i lim)
      res
      (if (or (zero? (rem i 3))
              (zero? (rem i 5)))
        (recur (+ i 1) (+ res i))
        (recur (+ i 1) res)))))

(defn ^long sol1f
  [^long lim]
  (->> (for [i (range 1 lim)
             :when (or (zero? (rem i 3))
                       (zero? (rem i 5)))]
         i)
       (reduce +)))

(defn ^long sol1g
  [^long lim]
  (let [res (atom 0)]
    (doseq [i (range 1 lim)
            :when (or (zero? (rem i 3))
                      (zero? (rem i 5)))]
      (swap! res #(+ % i)))
    @res))

