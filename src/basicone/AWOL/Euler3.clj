;EULER BATCH 3

(defn ubah [a] (Integer/parseInt a))

;No 22
(defn lettertonum [x] (cond (= x \A) 1                         (= x \B) 2                         (= x \C) 3                         (= x \D) 4
                        (= x \E) 5                         (= x \F) 6                         (= x \G) 7                         (= x \H) 8
                        (= x \I) 9                         (= x \J) 10                         (= x \K) 11                         (= x \L) 12
                        (= x \M) 13                         (= x \N) 14                         (= x \O) 15                         (= x \P) 16
                        (= x \Q) 17                         (= x \R) 18                         (= x \S) 19                         (= x \T) 20
                        (= x \U) 21                         (= x \V) 22                         (= x \W) 23                         (= x \X) 24
                        (= x \Y) 25                         (= x \Z) 26))
(defn wordtonum 
  ([x] (wordtonum x 0))
  ([x ans] (if (empty? x) ans (recur (rest x) (+ (lettertonum (first x)) ans)))))

;(def prob22 
 ; (->> (slurp "FINAL.txt")
  ;  (remove #(= \" %)))) ;NEED TO SORT THIS

(defn no22
  ([prob] (no22 prob 0 0 1))
  ([prob word ans no] (cond (empty? prob) (->> ans (+ (* word no)))
                        (= (first prob) \,) (recur (rest prob) 0 (->> ans (+ (* word no))) (inc no))
                        :else (recur (rest prob) (+ word (lettertonum (first prob))) ans no))))
