<<<<<<< Updated upstream
(ns basicone.sessthree.donna-exercise)

;;;A1. Reimplement Reduce Panjang
=======
;;; A.1 Reimplement Reduce Panjang
>>>>>>> Stashed changes

(defn new-reduce 
  ([f coll]
    (loop [[x & xs] coll res x]
      (if (first xs)
        (recur xs (f res (first xs)))
        res)))
  ([f acc coll]
    (loop [[x & xs] coll res acc]
      (if x
        (recur xs (f res x))
        res))))

<<<<<<< Updated upstream
;;;A2. Reimplement Reduce Multi-Arity beneran
=======
;;; A.2 Reimplement Reduce Multi-Arity beneran
>>>>>>> Stashed changes
(defn new-new-reduce
  ([f coll] (new-new-reduce f (first coll) (rest coll)))
  ([f acc coll]
    (loop [[x & xs] coll res acc]
      (if x
        (recur xs (f res x))
        res))))
<<<<<<< Updated upstream
      
;;; Contoh dari Sabda tentang Multy-Arity
(defn fibo-less
  ([lim]
    (fibo-less 1 0 lim))
  ([a b lim]
    (if (> a lim)
      []
      (cons b (fibo-less (+ a b) a lim)))))

;;;B1. Max-by
=======

; B.1 Max-By
>>>>>>> Stashed changes

(defn mapping-stuff
  [f coll]
  (loop [[x & xs] coll res {}]
    (if x
      (recur xs (assoc res (f x) x))
      res)))

(defn find-max
  [map]
  (map (reduce max (keys map))))

(defn max-by
  [f coll]
  (find-max (mapping-stuff f coll)))

<<<<<<< Updated upstream
;;;B2. New-Max-by
=======
; B.2 Max-by baru
>>>>>>> Stashed changes

(defn new-max-by
  [f [x & xs]]
  (if (first xs)
<<<<<<< Updated upstream
    (let [nmax (new-max-by f xs)]
      (if (> (f x) (f nmax)) x nmax))
    x))

;;;C1. Min-by
=======
    (let [nmax (max' f xs)]
      (if (> (f x) (f nmax)) x nmax))
    x))



; (min-by second [])
>>>>>>> Stashed changes

(defn find-min
  [map]
  (map (reduce min (keys map))))

(defn min-by
  [f coll]
  (find-min (mapping-stuff f coll)))

<<<<<<< Updated upstream
;;;C2. New-Min-by

=======
>>>>>>> Stashed changes
(defn new-min-by
  [f [x & xs]]
  (if (first xs)
    (let [nmin (new-min-by f xs)]
      (if (< (f x) (f nmin)) x nmin))
    x))

<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes
