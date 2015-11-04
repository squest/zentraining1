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

;NO46
(defn no46
  ([x] #(x %2 %1)))

;No82
(defn no82a [x & xs]
  (if (and (empty? xs) (not (coll? (first x)))) x
                                                (cons x (no82a xs))))

(defn no82a [x]
  (if (= some (coll? (#(map first %)) x))
    (cons (flatten (first x)) (map flatten (second x)))
    (map flatten x)))

;No90
(defn no90 [a b]
  (let [[x & xs] a]
    (if xs (cons (map #(vector x %) b) (no90 xs b))
           (map #(vector xs %) b))))

(defn no90b [a b]
  (if (empty? (rest a))
    (map #(vector (first a) %) b)
    (mapcat (#(vector (first a) %) b) (no90b (rest a) b))))

;No99
#(map read-string (map str (str (* %1 %2))))

;No107
(defn expt [n] #(reduce * (repeat n %)))


(remove #(contains? [1 2 3] %) (range 1 10))

