(ns basicone.dsapoetra.4clojure.p175)

;;153
#(apply distinct? (mapcat seq %))

;;156
(fn [v m] (zipmap m (repeat v)))

;;157
(fn [v]
  (map #(vector %1 %2) v (range )))

;;161
;;#{1 2}

;;162
;;->1

;;166
(fn [op a b]
  (cond
    (op a b) :lt
    (op b a) :gt
    :else :eq))

;;173
;;op arg