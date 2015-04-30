(ns basicone.sesstwo.guri_tugas)
(ns basicone.core)

(defn square
  [x]
  (* x x))

(defn gamp [y] (* y 10))

; (take n xs), (drop n xs), (product xs), (maxi xs), (mini xs), (revs xs), (removes elmt lst), (faktorial i), (expt a m).(





(defn sum 
  [xs]
  (if (empty? xs)
    0
    (+ (first xs) (sum (rest xs)))))

(defn tambah
  [[x & xs]]
  (if x (+ x (tambah xs)) 0))

(defn gtake
  [a [x & xs]]
  (if(<= a 0)
    []
   (cons x (gtake (dec a) xs))))

(defn gdrop
  [a [x & xs]]
  (if(<= a 0)
    xs
   (gdrop (dec a) xs)))

(defn gproduct 
  [x]
  (if (empty? x)
    1
    (* (first x) (gproduct (rest x)))))

(defn gmax 
  [[x & xs]]
  (if xs(let [maxnyaxs (gmax xs)]
        (if (> maxnyaxs x) 
          maxnyaxs x))
    x))


(defn gmin 
  [[x & xs]]
  (if xs(let [gmins (gmin xs)]
        (if (> x gmins) 
          gmins x))
    x))



(defn sqr-all2
  [xs]
  (loop [[l & ls] xs res []]
    (if l
      (recur ls (conj res (square l)))
      res)))

(defn myrev
  [ls]
  (loop [[x & xs] ls res []]
    (if x
      (recur xs (cons x res))
      res)))

(defn myrev2
  [ls]
  (loop [xs ls res []]
    (if (empty? xs)
      res
      (recur (butlast xs)
             (conj res (last xs))))))

(defn rev1 [xs]
  (if (empty? xs)
    []
    (cons (last xs) (rev1 (butlast xs)))))

(defn rev2 [xs]
  (if (empty? xs)
    []
    (conj (rev2 (rest xs)) (first xs))))

(defn gtakelooptest
  [a [x & xs]]
  (if(<= a 0)
    []
   (cons x (gtake (dec a) xs))))

(defn gtakeloop
  [nm vct]
  (loop [nmbr nm [x & xs] vct res []]
    (if nm
      (recur (dec nm) xs)
      res)))

(defn myrev
  [ls]
  (loop [[x & xs] ls res []]
    (if x
      (recur xs (cons x res))
      res)))

(defn sum-two
  [l1 l2]
  (loop [[x & xs] l1 [y & ys] l2 res []]
    (if x
      (recur xs ys (conj res (+ x y)))
      res)))










