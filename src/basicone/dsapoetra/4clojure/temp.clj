(ns basicone.dsapoetra.4clojure.temp)

(= __
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))


(fn prime [n]
  (take n (filter (fn p [x]
                    (empty? (filter #(zero? (rem x %)) (range 2 x))))
                  (range 2 Double/POSITIVE_INFINITY))))

(fn [s]
  (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))

﻿#(sort-by (fn [v](.toLowerCase v)) (re-seq #"\w+" %))

(fn  [& args]
  (fn [& args1]
    (map #(apply % args1) args)))

(fn [limit coll]
  (if (>= (count coll) limit)
    (cons (take limit coll) (recur limit (drop limit coll)))))

(fn perf-square [s]
  (let [nums (map #(Integer/valueOf %)  (clojure.string/split s #","))
        all-perf-sq  (set (map #(* % %) (range 100)))
        sq-nums (filter all-perf-sq nums)]
    (apply str (interpose "," sq-nums))))

(fn [s]
  (let [num-seq (map #(Integer/parseInt %) (re-seq #"\d+" s))]
    (apply str (interpose "," (filter (set (take-while #(<= % (apply max num-seq)) (map #(* % %) (range)))) num-seq)))))

(defn seq-type [coll]
  (let [base (empty coll)]
    (cond
      (= base {})  :map
      (= base #{}) :set
      (= base '()) (if (reversible? coll) :vector :list))))

#({{} :map #{} :set} (empty %) (if (reversible? %) :vector :list))


(= __
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))
factor
(fn [x]
  (if (= (reduce + (drop-last (filter #(zero? (rem x %)) (range 1 (inc x))))) (last (filter #(zero? (rem x %)) (range 1 (inc x)))))
    true
    false))

(fn [coll]
  (let [x (str (first (str coll)))]
    (cond
      (= "{" x) :map
      (= "c" x) :list
      (= "[" x) :vector
      (= "#" x) :set)))

(fn [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer/parseInt %))
       (filter (fn [x]
                 (let [r (int (Math/sqrt x))]
                   (= x (* r r)))))
       (interpose ",")
       (apply str)))

(fn [x](take x(filter #(= 2 (count (loop [x 1
                                          a %
                                          res '()]
                                     (if (> x a)
                                       res
                                       (if (= 0(mod a x))
                                         (recur (inc x) a (conj res x))
                                         (recur (inc x) a res))
                                       )
                                     ))) (iterate inc 1)
                      )))

(= (last ((fn [x](take x(filter #(= 2 (count (loop [x 1
                                                    a %
                                                    res '()]
                                               (if (> x a)
                                                 res
                                                 (if (= 0(mod a x))
                                                   (recur (inc x) a (conj res x))
                                                   (recur (inc x) a res))
                                                 )
                                               ))) (iterate inc 1)
                                )))
           100)) 541)


(fn first-n-prime [x]
  (take x (filter (fn [n] (= 2 (count ((filter #(zero? (rem n %)) (range 1 (inc n))))))) (range 1 10000000))))

#(clojure.string/replace % #"-(\w)" (fn [[a b]] (clojure.string/capitalize b)))

(fn [strings]
  (let [coll (seq (clojure.string/split strings #","))]
    (apply str (interpose "," (filter #(= 3 (count (str (Math/sqrt (Integer/parseInt %))))) coll)))))

(fn [x]
  (clojure.string/replace x #"-[a-z]" #(clojure.string/upper-case (second %))))


(fn [x]
  (letfn [(digits [n]
                  (for [y (iterate (partial * 10) 1) :while (<= y n)]
                    (rem (int (/ n y)) 10)))
          (sqr-sum [ds]
                   (reduce + (map #(* % %) ds)))]
    (let [r (some #{1 4} (iterate (comp sqr-sum digits) x))]
      (cond
        (= 1 r) true
        (= 4 r) false))))

(letfn [(num->digits [num]
                     (loop [n num res []]
                       (if (zero? n)
                         res
                         (recur (long (/ n 10)) (cons (mod n 10) res)))))
        (change [n]
                (apply + (map #(* % %) (num->digits n))))]
  (fn [init]
    (loop [curr init results #{}]
      (println curr " - " results)
      (cond
        (= 1 curr) true
        (results curr) false
        :else (let [new-n (change curr)]
                (println curr new-n)
                (recur new-n (into results [curr])))
        )))
  )

(fn ishappy
  ([num] (ishappy num #{}))
  ([num seen]
   (letfn  [
            (digits [num]
                    (map #(Character/digit % 10) (str num)))
            (nextnum [num]
                     (reduce + (map #(* % %) (digits num))))]
     (if (= 1 num) true
                   (if (contains? seen num) false
                                            (ishappy (nextnum num) (conj seen num)))))))

(fn happy? [num]
  (letfn [(digits [x] (map #(-> % str read-string) (str x)))
          (square [x] (map #(int (Math/pow % 2)) x))]
    (loop [x num visited #{}]
      (let [sum (reduce + (square (digits x)))]
        (cond
          (= 1 sum) true
          (contains? visited sum) false
          :else (recur sum (conj visited sum)))))))