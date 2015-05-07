; Reimplement Reduce

(defn new-reduce 
  ([f coll]
    (loop [f f [x & xs] coll res x]
      (if (first xs)
        (recur f xs (f res (first xs)))
        res)))
  ([f acc coll]
    (loop [f f [x & xs] coll res acc]
      (if x
        (recur f xs (f res x))
        res))))

; (max-by second []) => (max-by second [[1 2] [3 4] [0 10] [3 8]]) = [0 10]

(defn mapping-stuff
  [f coll]
  (loop [f f [x & xs] coll res {}]
    (if x
      (recur f xs (assoc res (f x) x))
      res)))

(defn find-max
  [map]
  (map (reduce max (keys map))))

(defn max-by
  [f coll]
  (find-max (mapping-stuff f coll)))

; (min-by second [])

(defn find-min
  [map]
  (map (reduce min (keys map))))

(defn min-by
  [f coll]
  (find-min (mapping-stuff f coll)))

