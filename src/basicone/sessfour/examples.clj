(ns basicone.sessfour.examples)

(defn sum
  [[x & xs]]
  (if x (+' x (sum xs)) 0))

(defn prod
  [[x & xs]]
  (if x (*' x (prod xs)) 1))

(defn sum'
  ([xs] (sum' xs 0))
  ([[x & xs] res] (if x (recur xs (+ res x)) res)))

(defn rev
  ([xs] (rev xs []))
  ([[x & xs] res]
   (if x (rev xs (cons x res)) res)))

(defn prime?
  [n]
  (cond
    (< n 2) false
    (= 2 n) true
    (even? n) false
    :else (let [lim (int (Math/sqrt n))]
            (loop [i (int 3)]
              (cond
                (> i lim) true
                (zero? (rem n i)) false
                :else (recur (+ i 2)))))))

(defn odd-prime?
  ([^long n]
   (odd-prime? n 3))
  ([^long n ^long i]
   (if (> i (quot n i))
     true
     (if (zero? (rem n i))
       false
       (recur n (+ i 2))))))

(defn sum-primes
  ([^long lim]
   (sum-primes 3 2 lim))
  ([^long i ^long res ^long lim]
   (cond
     (> i lim) res
     (odd-prime? i) (recur (+ i 2) (+ i res) lim)
     :else (recur (+ i 2) res lim))))

