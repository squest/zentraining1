(ns basicone.dsapoetra.4clojure.p100)

;;81
(fn [a b] (into #{} (filter #(contains? a %) b)))

;;83
(fn [& bools] (and (reduce #(or %1 %2) bools) (not (reduce #(and %1 %2) bools))))

;;88
(fn [a b]
  (let [dis (clojure.set/intersection a b)
        a-sub (clojure.set/difference a dis)
        b-sub (clojure.set/difference b dis)]
    (clojure.set/union a-sub b-sub)))

;;90
(fn cart [x y]
  (set (for [a x b y] [a b])))

;;95
(fn is-binary? [tree]
  (cond
    (false? tree) false
    (not (coll? tree)) true
    (not= (count tree) 3) false
    :else (and (is-binary? (second tree)) (is-binary? (nth tree 2)))))

;;96
(fn [t]
  ((fn mir? [l r]
     (if (or (= nil l r)
             (and (= (first l) (first r))
                  (mir? (second l) (last r))
                  (mir? (last l) (second r))))
       true false))
    (second t) (last t)))

;;97
(fn [n] (nth (iterate #(vec (map + (conj % 0) (cons 0 %))) '[1]) (dec n)))

;;99
(fn prod-dig [a b]
  (let [prod (* a b)
        len (count (str prod))]
    (map #(rem % 10) (reverse (take len (iterate #(quot % 10) prod))))))


;;100
(fn [& args]
  (letfn [(gcd [x y]
               (let [a (max x y)
                     b (min x y)
                     m (mod a b)]
                 (if (zero? m)
                   b
                   (recur b m))))
          (lcm [a b]
               (/ (* a b) (gcd a b)))]
    (reduce lcm args)
    ))