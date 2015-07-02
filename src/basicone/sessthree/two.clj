(ns basicone.sessthree.two
  (:require [taoensso.carmine :as car]))

(defn faktorial
  [i]
  (if (= i 0)
    1
    (*' i (faktorial (dec i)))))

(defn add
  ([x] x)
  ([x y] (+ x y))
  ([x y z] (+ x y z)))

(defn fak2
  ([i] (fak2 i 1))
  ([i res]
   (if (= i 0)
     res
     (recur (dec i) (*' res i)))))

(defn square
  [x]
  (* x x))

(defn take-rec
  ([n coll] (take-rec n coll []))
  ([n [x & xs] res]
   (if (zero? n)
     res
     (recur (dec n) xs (conj res x)))))

;; Tree Recursion -> clojure

(defn fak
  [i]
  (if (= 0 i)
    1
    (* i (fak (dec i)))))

(defn fibo
  [i]
  (cond (= i 1) 1
        (= i 2) 2
        :else (+ (fibo (dec i))
                 (fibo (- i 2)))))

;;; fibo 6 => fibo 5 + fibo 4
;; fibo 5 => fibo 4 + fibo 3
;; fibo 4 => fibo 3 + fibo 2

(def mfibo
  (memoize
    (fn [i]
      (cond (= i 1) 1
            (= i 2) 2
            :else (+' (mfibo (dec i))
                      (mfibo (- i 2)))))))

(defn expt
  [a m]
  (cond (== m 0) 1
        (== m 1) a
        :else (let [half (expt a (quot m 2))]
                (if (even? m)
                  (*' half half)
                  (*' a half half)))))


(defn lfibo
  ([lim] (lfibo 2 1 3 lim))
  ([a b i lim]
   (if (> a lim) i (recur (+' a b) a (+ i 1) lim))))

(def red-server
  {:pool {}
   :spec {:host "localhost" :port 6379}})

(defn f
  ([i] (if (= i 0)
         (cons 0 (lazy-seq (f i 1)))
         (let [n (int (Math/ceil (Math/log10 i)))]
           (lazy-cat (f i n)))))
  ([i n]
    (if (even? n)
      (let [s (->> (range (long (Math/pow 10 (dec n)))
                          (long (Math/pow 10 n)))
                   (map #(->> (str %)
                              reverse
                              (apply str)
                              (str %)
                              bigint))
                   (drop-while #(< i %)))]
        (lazy-cat s (lazy-seq (f (long (Math/pow 10 n)) (+ n 1)))))
      (let [s (->> (range (long (Math/pow 10 (dec n)))
                          (long (Math/pow 10 n)))
                   (map #(->> (str %)
                              reverse
                              rest
                              (apply str)
                              (str %)
                              bigint))
                   (drop-while #(< i %)))]
        (lazy-cat s (lazy-seq (f (long (Math/pow 10 n)) (+ n 1))))))))

























