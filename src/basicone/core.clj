(ns basicone.core)

(defn square 
  [x]
  (* x x))

(def m {:nama "Joni" :umur "uzur"})

(defn cube 
  [x]
  (* x x x))

(def m [1 2 3])

(defn square 
  [x]
  (* x x))

(def sqr (fn [x] (* x x)))
(defn sqr [x] (* x x))

(defn mutlak
  [x]
  (if (>= x 0)
    x
    (- x)))

(defn nambah10atau5
  [x]
  (+ x (if (even? x) 10 5)))

(defn gadungan 
  [x]
  (cond 
    (== x 0) 0
    (> x 0) 10
    "selain itu : " -10))

(defn sum 
  [xs]
  (if (empty? xs)
    0
    (+ (first xs) (sum (rest xs)))))

(defn sum'
  [[x & xs]]
  (if x (+ x (sum' xs)) 0))

(defn ngalivalue
  [{:keys [a b c]}]
  (* a b c))


;; (take n xs), (drop n xs), (product xs), (maxi xs), (mini xs), (revs xs), (removes elmt lst), (faktorial i), (expt a m).






;; Da, gue udah selesai. 

















