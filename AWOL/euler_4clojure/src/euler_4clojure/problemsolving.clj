(ns euler-4clojure.problemsolving
  (:require
    [euler-4clojure.core :as core]
    [clojure.string :as cs]
    [clojure.edn :as edn])) /


;ffirst, map-indexed, keep, keep-indexed,
;(using #{} as lambda function in map/some/filter/remove),
;group-by, partition, partition-by, get-in, assoc-in, update-in, split-at, split-with, take-while, drop-while,
;drop-last, interleave, interpose, comp, juxt, partial, complement,
;zipmap, mapcat, take-last, reductions, merge, merge-with, select-keys,
;(map destructuring {:keys [some-key another-key]})

;(def my-future (future (/ 1 0))

;no21
(defn amic? [x]
  (let [a (reduce + (core/divisor x))]
    (if (and (not (= x a))
             (= x (reduce + (core/divisor a)))) true false)))

(defn no21 [x]
  (->> (filter amic? (range 1 (inc x)))
       (reduce +)))

;No 23
(defn abund? [x]
  (> (reduce + (core/divisor x)) x))

(defn abunsum? [x]
  (cond (odd? x) false
        (abund? (quot x 2)) true
        :else false))

(defn no23 [max]
  (->> (range 1 (inc max))
       (reduce +)))

;No26
(defn remy [a b]
  (cond (= a b) 0
        (> b a) (rem (* 10 a) b)
        :else (rem a b)))

(defn no26
  ([b] (no26 1 b [b]))
  ([a b ans]
   (let [x (remy a b)]
     (cond (some #(= x %) ans) ans
           (= 0 x) (conj ans 0)
           :else (recur x b (conj ans x))))))

(defn find26 [d]
  (let [x (map no26 (range 1 (inc d)))
        maxy (reduce max (map count x))]
    (filter #(= maxy (count %)) x)))

;No29
(defn no29
  ([start end] (no29 (range start (inc end)) start end [] []))
  ([col start end ans temp] (cond (> start (inc end)) (count ans)
                                  (not (empty? temp)) (if (some #(= (first temp) %) ans)
                                                        (recur col start end ans (rest temp))
                                                        (recur col start end (cons (first temp) ans) (rest temp)))
                                  :else (recur col (inc start) end ans (map #(core/exp start %) col)))))

;No30
(defn ispower? [a]
  (= a (reduce + (map #(core/exp % 5) (core/numtodig a)))))

(defn no30
  ([a] (no30 a 0 1500000))
  ([a ans end] (cond (> a end) ans
                     (ispower? a) (recur (inc a) (+ a ans) end)
                     :else (recur (inc a) ans end))))

;No41
(defn pandigital? [x]
  (let [a (core/numtodig x)]
    (and (= (count a)
            (count (set a)))
         (= (sort a) (range 1 (inc (count a)))))))

(defn no41 [x]
  (cond (= x 0) 0
        (and (pandigital? x) (core/prime? x)) x
        :else (recur (dec x))))

;No47
(defn consec? [x coll]
  (if (= (range x) (map #(- % (first coll)) coll)) true false))

(defn no47
  ([x] (no47 x (->> (range 134000 135000)
                    (filter #(= x (count (core/primefactor %)))))))
  ([x coll] (cond (consec? x (take x coll)) (take x coll)
                  (> x (count coll)) "Set higher upper limit!"
                  :else (recur x (rest coll)))))

;No52
(defn no52 [x]
  (if (every? true? (->> (map #(* x %) [1 2 3 4 5 6])
                         (map core/numtodig)
                         (map sort)
                         (map #(= (sort (core/numtodig x)) %))))
    x
    (recur (inc x))))

;No63
(defn no63
  ([a] (no63 a 1 0))                                        ; a is start
  ([a n ans]
   (let [powcount (->> (core/exp a n)
                       (core/numtodig)
                       (count))]
     (cond (> (count (core/numtodig (core/exp 2 n))) n) ans
           (> powcount n) (recur 1 (inc n) ans)
           (= powcount n) (recur (inc a) n (inc ans))
           :else (recur (inc a) n ans)))))

;No74
(defn chainfac [x]
  (->> (core/numtodig x)
       (map core/factorial)
       (reduce +)))

(defn chain74 [x]
  (->> (take 60 (iterate chainfac x))))

(defn unique? [x]
  (if (= (count (chain74 x)) (count (set (chain74 x)))) true false))

(defn no74 [max]
  (->> (range 1 max)
       (map #(unique? %))
       (filter #(= true %))
       (count)))

;No 92 chain ;"Elapsed time: 983616.393211 msecs"
(defn which92? [n]
  (if (or (= n 89) (= n 1)) n
                            (->> (core/numtodig n)
                                 (map core/square)
                                 (reduce +)
                                 (recur))))
(defn no92 [max]
  (->> (range 1 max)
       (map which92?)
       (filter #(= 89 %))
       (count)))



;====================================================================================================

;No22
(->> (slurp "resources/test.edn")
     (remove #(= \, %))
     (apply str)
     (vector)
     (sort-by str))

;No55
(defn rvrs [x]
  (->> (core/numtodig x)
       (reverse)
       (apply str)
       (core/ubah)))

(defn plusmirror [x]
  (+' x (rvrs x)))

(defn lych?
  ([x] (lych? (plusmirror x) 0))
  ([x a] (cond (> a 49) true
               (core/palindrome? x) false
               :else (recur (plusmirror x) (inc a)))))

(defn no55
  ([max] (no55 (dec max) 0))
  ([max ans] (cond (< max 10) ans
                   (lych? max) (recur (dec max) (inc ans))
                   :else (recur (dec max) ans))))





;No95
(defn no55
  ([x] (no55 x x 0 0 0))
  ([x temp xlong ans long]
   (let [temp (reduce + (core/divisor temp))]
     (cond (= 0 x) ans
           (> temp 1000) (recur (dec x) (dec x) 0 ans long)
           (= temp x) (if (>= xlong long) (recur (dec x) (dec x) 0 x xlong)
                                          (recur (dec x) (dec x) 0 ans long))
           :else (recur x temp (inc xlong) ans long)))))

;(defn amic? [x]  (let [test (reduce + (core/divisor x))]    (if (= test (reduce + (core/divisor test))) true false)))

