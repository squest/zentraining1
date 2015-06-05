(ns basicone.dsapoetra.4clojure.p75)

;;util


;;51
;;[1 2 3 4 5]

;;52
;;[c e]

;;55
(fn [coll]
  (reduce (fn [counts x]
            (assoc counts x (inc (get counts x 0))))
          {} coll))

;;56
(fn distinct2 [sequ]
     (if (empty? sequ) sequ (cons (first sequ) (distinct2 (filter #(not= % (first sequ) ) (rest sequ) )))))

;;57
;;'(5 4 3 2 1)

;;58
(fn comb [& funcs]
  (fn [& args]
    (first
      (reduce #(vector (apply %2 %1)) args (reverse funcs )))))


;;61
#(apply assoc {} (interleave %1 %2))

;;62
(fn ite [f x] (cons x (lazy-seq (ite f (f x)))))

;;63
(fn group [f col]
  (reduce #(assoc %1 (f %2) (conj (apply vector (%1 (f %2))) %2)) {} col))

;;64
;; -> +

;;66
(fn gcd[a b]
  (if (= b 0)
    a
    (recur b (mod a b))
    ))

;;68
;;[7 6 5 4 3]

;;71
;;last

;;72
;;reduce +