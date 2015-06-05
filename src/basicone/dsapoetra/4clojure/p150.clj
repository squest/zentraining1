(ns basicone.dsapoetra.4clojure.p150)

;;126
;;Class

;;128
(fn poke [s]
  (let [suit-map {\H :heart, \C :club, \D :diamond, \S :spades}
        rank-map {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5,
                  \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}
        ]
    {:suit (suit-map (first s)), :rank (rank-map (last s))}
    ))

;;134
#(and (contains? %2 %1) (nil? (%2 %1)))



;;135
(fn [& exp]
  (reduce #(if (fn? %1) (%1 %2) (partial %2 %1)) identity exp))

;;143
(fn cross-prod [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

;;145
;;'(1 5 9 13 17 21 25 29 33 37)

;;146
#(into {} (for [[k m] % [p n] m] [[k p] n]))

;;147
(fn [s]
  (let [nextrow (fn [row](map +' (cons 0 row) (concat row [0])))]
    (iterate nextrow s)))

;;Feeling lo, itu cowok umur berapa?