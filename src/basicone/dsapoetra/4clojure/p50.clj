(ns basicone.dsapoetra.4clojure.p50)

;;26
(fn [x]
  (map (fn fib[n] (if (= n 1) 1 (if (<= n 0) 0 (+ (fib (- n 1)) (fib (- n 2))))))
       (range 1 (+ x 1))))

;;27
(fn [x] (= (reverse (reverse x)) (reverse x)))

;;28
(fn [x] (filter (complement sequential?)
                (rest (tree-seq sequential? seq x))))

;;29
(fn [string] (apply str (filter (fn [x] (re-matches #"[A-Z]" (str x))) string)))

;;30
(fn [x] (map first (partition-by identity x)))

;;31
(fn [x] (partition-by identity x))

;;32
(fn [s] (mapcat identity
                (map (fn [x] (cons x (cons x '()))) s)))

;;33
(fn [list num]
  (mapcat #(repeat num %) list))

;;34
#(take (- %2 %1) (iterate inc %1))

;;35
;;->7

;;36
;;[x 7, y 3, z 1]

;;37
;;"ABC"

;;38
(fn [x & xs]
  (reduce #(if (< %1 %2) %2 %1) x xs))

;;39
#(mapcat vector %1 %2)

;;40
(fn [sep coll]
  (drop-last (mapcat vector coll (repeat sep))))

;;41
(fn [coll n]
  (flatten
    (concat
      (map #(drop-last %) (partition n coll))
      (take-last (rem (count coll) n) coll))))

;;42
(fn [n]
  (apply * (range 1 (inc n))))


;;43
(fn [coll n]
  (apply map list (partition n coll)))

;;44
(fn [n coll]
  (let [ntime (if (neg? n) (- n) n)
        lshift #(concat (rest %) [(first %)])
        rshift #(cons (last %) (drop-last %))]
    ((apply comp (repeat ntime (if (neg? n) rshift lshift))) coll)))

;;45
;;'(1 4 7 10 13)

;;46
(fn [f] (fn [& args] (apply f (reverse args))))

;;47
;;4

;;48
;;6

;;49
#(cons (take %1 %2) (list (drop %1 %2)))

;;50
#(vals (group-by type %))