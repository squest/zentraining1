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

(defn gdroploop
  [drop-number drop-vector]
  (loop [dn drop-number [x & xs] drop-vector]
    (if (= dn 1)
      xs
      (recur (dec dn) xs))))

(defn gproductloop
  [x-product]
  (loop [[x & xs] x-product res 1]
    (if 
      x(recur xs (* res x))
      res))))

(defn gmaxloop
  [max-vector]
  (loop [[x & xs] max-vector res []]
    (if
      xs (if (> x xs) 
            (conj (rest res) x) 
            (recur xs))
      res)))

;; 

;; max by (maxby second [[2 3] [6 7] [9 2]]) = [6 7]


;; min by (minby second [[2 3] [6 7] [9 2]]) = [9 2]


;; reduce




