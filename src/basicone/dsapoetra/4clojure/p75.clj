(ns basicone.dsapoetra.4clojure.p75)

;;util

;;56

(fn distinct2 [sequ]
     (if (empty? sequ) sequ (cons (first sequ) (distinct2 (filter #(not= % (first sequ) ) (rest sequ) )))))

;;
