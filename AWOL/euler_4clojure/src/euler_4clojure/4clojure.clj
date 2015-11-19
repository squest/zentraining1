(ns euler-4clojure.4clojure)

(defn no62 [fun n] (cons n
                     (lazy-seq (no62 fun (fun n)))))

;ffirst, map-indexed, keep, keep-indexed,
;(using #{} as lambda function in map/some/filter/remove),
;group-by, partition, partition-by, get-in, assoc-in, update-in, split-at, split-with, take-while, drop-while,
;drop-last, interleave, interpose, comp, juxt, partial, complement,
;zipmap, mapcat, take-last, reductions, merge, merge-with, select-keys,
;(map destructuring {:keys [some-key another-key]})

;No43
(defn no43 [col n]
  (let [n2 (count (partition n col))]
    (->> (partition n col)
         (apply interleave)
         (partition n2))))

;No46
(defn no46
  ([x] #(x %2 %1)))

;No50
#(vals (group-by type %))

;No55
#(let [a (group-by identity %)]
      (apply assoc {}
             (interleave (keys a) (map count (vals a)))))
;No58


;No59
(defn no59 [& fs]
    (fn [& xs]
        (map #(apply % xs) fs)))

;No60
(fn no60
    ([f a [x & xs]] (cons a (lazy-seq (when x (no60 f (f a x) xs)))))
    ([f [x & xs]] (no60 f x xs)))


;No70
(defn no70 [x]
  (sort-by #(str (clojure.string/lower-case %))
           (clojure.string/split x #"!|\.|\s")))

;No74
(defn no74 [x]
  (->> (map #(Integer/parseInt %)
            (clojure.string/split x #","))
       (filter #(= \0 (last (str (Math/sqrt %)))))
       (interpose ",")
       (apply str)))

;No80
(defn no80 [x]
  (->> (range 1 x)
       (filter #(= 0 (mod x %)))
       (reduce +)
       (= x)))

;No82
(defn no82a [x & xs]
  (if (and (empty? xs) (not (coll? (first x)))) x
                                                (cons x (no82a xs))))

(defn no82a [x]
  (if (= some (coll? (#(map first %)) x))
    (cons (flatten (first x)) (map flatten (second x)))
    (map flatten x)))

;No90
#(set (for [x (vec %1) y (vec %2)] [x y]))

;No91
(defn no91 [x]
    (let [[a b & c] (vec x)]
         (if b (if (= (last a) (first b))
                 (no91 (rest x))
                 false)
               false)))

;No99
#(map read-string (map str (str (* %1 %2))))

;No107
(defn expt [n] #(reduce * (repeat n %)))


(remove #(contains? [1 2 3] %) (range 1 10))

