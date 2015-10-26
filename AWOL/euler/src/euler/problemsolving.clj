(ns euler.problemsolving
  (:require
    [euler.core :as core]
    [clojure.string :as cs]
    [clojure.edn :as edn]))/


;ffirst, map-indexed, keep, keep-indexed,
;(using #{} as lambda function in map/some/filter/remove),
;group-by, partition, partition-by, get-in, assoc-in, update-in, split-at, split-with, take-while, drop-while,
;drop-last, interleave, interpose, comp, juxt, partial, complement,
;zipmap, mapcat, take-last, reductions, merge, merge-with, select-keys,
;(map destructuring {:keys [some-key another-key]})

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

;No63
(defn no63
  ([a] (no63 a 1 0))    ; a is start
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

;No 23
(defn abund? [x]
  (> (reduce + (core/divisor x)) x))

(defn abunsum? [x]
  (cond (odd? x) false
        (abund? (quot x 2)) true
        :else false))

(defn no23 [max]
  (->> (range 1 (inc max))
       (filter #(not (abunsum? %)))
       (reduce +)))

;No47
(defn consec? [x]
      (cond (> 2 (count x)) true
            (= 1 (- (second x) (first x))) true
            :else false))

(defn primefac? [x num]
  (cond (> num (count (core/primefactor x))) false
        (= num (count (core/primefactor x))) true
        :else false))

(defn no47
      ([x num] (no47 x num []))
      ([x num ans] (cond (= x 200000) x                     ;-> checked until 40000
                         (= (count ans) num) (if (consec? ans) ans (recur (last ans) num (vec (nthnext ans 1))))
                         (primefac? x num) (cond (empty? ans) (recur (inc x) num [x])
                                                 (consec? (conj [(last ans)] x)) (recur (inc x) num (conj ans x))
                                                 :else (recur (inc x) num [x]))
                         :else (recur (inc x) num ans))))

(defn consecx? [num col]
  (->> (if (consec? (take num col)) (take num col)
                                  (recur (rest col) num))))

(defn no47b [max num]
  (->> (range 1 (inc max))
       (filter #(primefac? % num))
       ))




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
  ([x a] (cond (> a 49) true (core/palindrome? x) false
               :else (recur (plusmirror x) (inc a)))))

(defn no55
  ([max] (no55 (dec max) 0))
  ([max ans] (cond (< max 1) ans
                   (lych? max) (recur (dec max) (inc ans))
                   :else (recur (dec max) ans))))





;No95
(defn amic? [x]
  (let [test (reduce + (core/divisor x))]
    (if (= test (reduce + (core/divisor test))) true false)))

