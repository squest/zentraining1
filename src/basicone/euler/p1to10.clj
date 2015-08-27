(ns basicone.euler.p1to10
  (:require [clojure.string :as cs]))

(defn time-it
  [f input]
  (println (f input))
  (dotimes [i 10]
    (time (f input))))

(defn sol1
  [lim]
  (- (+ (reduce + (range 3 lim 3))
        (reduce + (range 5 lim 5)))
     (reduce + (range 15 lim 15))))

(defn ^long sol1a
  [^long lim]
  (loop [i (int 1) res (int 0)]
    (if (> i lim)
      res
      (if (or (== 0 (rem i 3))
              (== 0 (rem i 5)))
        (recur (+ i 1) (+ res i))
        (recur (+ i 1) res)))))

(defn lfibo
  [lim]
  (->> (iterate #(let [[a b] %]
                  [(+ a b) a]) [1 0])
       (map first)
       (take-while #(<= % lim))))

(defn ^long sol2
  [^long lim]
  (->> (lfibo lim)
       (filter #(even? %))
       (reduce +)))

(defn ^long sol2a
  [^long lim]
  (loop [a (int 1) b (int 0) res (int 0)]
    (if (> a lim)
      res
      (if (even? a)
        (recur (+ a b) a (+ res a))
        (recur (+ a b) a res)))))


(defn ^boolean odd-prime?
  [^long n]
  (let [lim (int (Math/sqrt n))]
    (loop [i (int 3)]
      (if (> i lim)
        true
        (if (== 0 (rem n i))
          false
          (recur (+ i 2)))))))

(defn ^long sol3
  [^long tar]
  (loop [i (int 3) n tar]
    (if (odd-prime? n)
      n
      (if (== 0 (rem n i))
        (if (odd-prime? i)
          (let [nn (loop [nn n]
                     (if (== 0 (rem nn i))
                       (recur (quot nn i))
                       nn))]
            (recur (+ i 2) nn))
          (recur (+ i 2) n))
        (recur (+ i 2) n)))))

(defn ^long sol3a
  [^long tar]
  (loop [i (int 3) n tar]
    (if (== 0 (rem n i))
      (if (odd-prime? i)
        (let [nn (loop [nn n]
                   (if (== 0 (rem nn i))
                     (recur (quot nn i))
                     nn))]
          (if (odd-prime? nn)
            nn
            (recur (+ i 2) nn)))
        (recur (+ i 2) n))
      (recur (+ i 2) n))))

(defn ^longs numcol
  [^long n]
  (loop [tar (int n) res '()]
    (if (< tar 10)
      (conj res tar)
      (recur (quot tar 10)
             (conj res (rem tar 10))))))

(defn ^boolean palin?
  [^longs xs]
  (= xs (reverse xs)))

(defn ^long sol4
  [^long lim]
  (->> (for [i (range 900 lim)
             j (range 900 lim)
             :when (not= i j)
             :let [tmp (* i j)]]
         (if (palin? (numcol tmp)) tmp nil))
       (keep identity)
       (apply max)))

(defn ^long sol4a
  [^long lim]
  (loop [i (int lim) maxi (int 0)]
    (if (< i 900)
      maxi
      (let [tmp (loop [j (int (- i 1)) nmaxi maxi]
                  (if (< j 900)
                    nmaxi
                    (let [tmpi (* i j)]
                      (if (palin? (numcol tmpi))
                        (if (> tmpi nmaxi)
                          (recur (- j 1) tmpi)
                          (recur (- j 1) nmaxi))
                        (recur (- j 1) nmaxi)))))]
        (recur (- i 1) (max maxi tmp))))))

(defn ^long sol5
  [^long lim]
  (let [refs (into-array (range (+ lim 1)))]
    (loop [i (int 2)]
      (if (> i lim)
        (reduce * (rest (into [] refs)))
        (do (let [itmp (aget refs i)]
              (loop [j (int (* 2 i))]
                (when (<= j lim)
                  (aset refs j (quot (aget refs j) itmp))
                  (recur (+ j i)))))
            (recur (+ i 1)))))))

(defn ^long sol7
  [^long n]
  (->> (iterate #(+ % 2) 3)
       (filter odd-prime?)
       (drop (- n 2))
       first))

(defn ^long sol7a
  [^long n]
  (let [lim (* n 12)
        llim (int (Math/sqrt lim))
        primes (boolean-array (+ lim 1) true)]
    (loop [i (int 3) cur (int 2) ctr (int 1)]
      (if (== ctr n)
        cur
        (if (<= i llim)
          (if (aget primes i)
            (do (loop [j (int (* i i))]
                  (when (<= j lim)
                    (aset primes j false)
                    (recur (+ j i i))))
                (recur (+ i 2) i (+ ctr 1)))
            (recur (+ i 2) cur ctr))
          (if (aget primes i)
            (recur (+ i 2) i (+ ctr 1))
            (recur (+ i 2) cur ctr)))))))

(defn ^long sol8
  [asal]
  (->> (slurp asal)
       (cs/split-lines)
       (apply concat)
       (apply str)
       (map str)
       (map read-string)
       (iterate rest)
       (take-while not-empty)
       (filter #(>= (count %) 13))
       (map #(take 13 %))
       (map #(reduce * %))
       (apply max)))

(defn ^long sol8a
  [fname]
  (let [data (->> (slurp fname)
                  (cs/split-lines)
                  (apply concat)
                  (apply str)
                  (map str)
                  (map read-string))]
    (loop [[x & xs] data maxi 0]
      (let [tmp (cons x (take 12 xs))]
        (if (< (count tmp) 13)
          maxi
          (let [tmpi (reduce * tmp)]
            (recur xs (max maxi tmpi))))))))

(defn ^long sol9
  [^long lim]
  (let [sqr #(* % %)]
    (first
      (for [a (range 3 (inc (quot lim 4)))
            b (range (+ a 1) (quot lim 2))
            :let [c (- 1000 (+ a b))]
            :when (and (pos? c)
                       (== (sqr c) (+ (sqr a) (sqr b))))]
        (* a b c)))))

(defn ^long sol10
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        primes (boolean-array (+ lim 1) true)]
    (loop [i (int 3) res (int 2)]
      (if (> i lim)
        res
        (if (<= i llim)
          (if (aget primes i)
            (do (loop [j (int (* i i))]
                  (when (<= j lim)
                    (aset primes j false)
                    (recur (+ j i i))))
                (recur (+ i 2) (+ res i)))
            (recur (+ i 2) res))
          (if (aget primes i)
            (recur (+ i 2) (+ i res))
            (recur (+ i 2) res)))))))


;; 1 2 3 4 5 6 7 8 9 10
;; 1 2 3 2 5 3 7 4 9 5
;; 1 2 3 2 5 1 7 4 3 5
;; 1 2 3 2 5 1 7 2 3 5

































