(ns basicone.am)

(defn expt
  [a m]
  (if (== m 0) a (* a (expt a (dec m)))))

(defn fibo
  [i]
  (cond (== i 1) (do (print i " ")
                     1)
        (== i 2) (do (print i " ")
                     2)
        :else (do (print i " ")
                  (+ (fibo (- i 1))
                     (fibo (- i 2))))))

(defn sum
  [[x & xs]]
  (if x (+ x (sum xs)) 0))

(defn expt-1
  [a m]
  (cond (== m 0) 1
        (== m 1) a
        :else (let [res (expt-1 a (quot m 2))]
                (if (even? m)
                  (*' res res)
                  (*' a res res)))))

(defn expt-3
  [a m]
  (cond (== m 0) 1
        (== m 1) a
        :else (if (even? m)
                (*' (expt-3 a (quot m 2))
                   (expt-3 a (quot m 2)))
                (*' a
                   (expt-3 a (quot m 2))
                   (expt-3 a (quot m 2))))))

(def expt-2
  (memoize
   (fn [a m]
     (cond (== m 0) 1
           (== m 1) a
           :else (if (even? m)
                   (*' (expt-2 a (quot m 2))
                      (expt-2 a (quot m 2)))
                   (*' a
                       (expt-2 a (quot m 2))
                       (expt-2 a (quot m 2))))))))

(defn add
  [n]
  #(+ n %))

(defn sum
  [[x & xs]]
  (if x (+ x (sum xs)) 0))

(defn prod
  [[x & xs]]
  (if x (*' x (prod xs)) 1))

(defn kuadratin-semua
  [[x & xs]]
  (if x
    (conj (kuadratin-semua xs)
          (* x x))
    []))

(defn cube-in-semua
  [[x & xs]]
  (if x
    (conj (cube-in-semua xs)
          (* x x x))
    []))

(defn memap
  [f [x & xs]]
  (if x (conj (memap f xs) (f x)) []))
















