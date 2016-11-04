(ns euler-4clojure.part2)

(defn no148 [a b c]
  (let [n #(quot (-' a 1) %)
        f #(*' % (/ (*' (n %) (inc (n %))) 2))]
    (->> (+' (f b) (f c))
         (-> (-' (f (*' b c)))))))

(defn no96 [[a b c]]
  (let [s #(cons (first %) (sort [(second %) (last %)]))
        x (if (coll? b) (s b) b)
        y (if (coll? c) (s c) c)]
    (cond (and (= x y) (coll? x)) (no96 x)
          (= x y) true
          :e false)))

(defn no53 [x]
  (let [a (->> x
               (map-indexed vector)
               (partition-by #(apply - %)))
        m (apply max (map count a))
        z (->> (filter #(= (count %) m) a)
               (first)
               (map second))]
    (if (= 1 m) [] z)))

(defn no150 [x]
  (filter #(let [a (seq (str %))]
            (= a (reverse a))) (iterate inc x)))

(fn [x]
  (let [a (->> (group-by sort x)
               (vals)
               (map set))
        m (map count a)]
    (if (some #(< % (apply max m)) m)
      (set [(apply max-key count l)])
      (set l))))

(take 3
      ((fn [x]
         (iterate
           #(flatten (vec (let [n (partition-by identity %)]
                            (zipmap
                              (map count n)
                              (map last n)))))
           x)) [1]))