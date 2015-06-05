(ns basicone.sessthree.one)

<<<<<<< Updated upstream
;; ayo woi

;; Topic : higher order function

=======
>>>>>>> Stashed changes
(defn sum
  [[x & xs]]
  (if x (+ x (sum xs)) 0))

(defn product
  [[x & xs]]
  (if x (* x (product xs)) 1))

(defn square-all
  [[x & xs]]
  (if x (cons (* x x) (square-all xs)) []))

(defn minus-all
  [[x & xs]]
  (if x (cons (- x) (minus-all xs)) []))

(defn memap
  [f [x & xs]]
  (if x (cons (f x) (memap f xs)) []))

(defn square [x] (* x x))
(defn cube [x] (* x x x))

(defn adder
  [n]
  #(+ n %))

<<<<<<< Updated upstream
;; 1. re-implement reduce (reduce #(whatever %1 %2) xs)
;; 2. bikin function max-by & min-by -> ex. (max-by f xs)

;; ini bagian iseng

(defn prime?
  [^long p]
  (let [lim (int (Math/sqrt p))]
    (loop [i (int 3)]
      (cond (> i lim) true
            (even? i) false
            (== 0 (rem p i)) false
            :else (recur (+ i 2))))))

(def sumprime (ref 2))

(defn pprimes
  [a b c d]
  (dosync (pvalues (doseq [i (range 1 a)
                           :when (prime? i)]
                     (dosync (alter sumprime + i)))
                   (doseq [i (range a b)
                           :when (prime? i)]
                     (dosync (alter sumprime + i)))
                   (doseq [i (range b c)
                           :when (prime? i)]
                     (dosync (alter sumprime + i)))
                   (doseq [i (range c d)
                           :when (prime? i)]
                     (dosync (alter sumprime + i))))
          @sumprime))

(defn sort-luar-dalem
  [fg fs maps]
  (->> (group-by fg maps)
       (sort-by #(:somekey (key %)))
       (map #(vector (key %) (sort-by fs (val %))))
       (into {})))

(defn max'
  [[x & xs]]
  (if (first xs)
    (let [nmax (max' xs)]
      (if (> x nmax) x nmax))
    x))

(defn qsort
  [coll]
  (if (empty? coll)
    []
    (let [[x & xs] coll
          smaller (filter #(<= % x) xs)
          larger (filter #(> % x) xs)]
      (concat (qsort smaller) [x] (qsort larger)))))

(defn brojol
  [a & ar]
  (reduce #(let [[f n] %2] (f %1 n)) a (partition 2 ar)))
=======
;; 

>>>>>>> Stashed changes





