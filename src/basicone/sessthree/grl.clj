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
    (if x 
      (recur xs (* res x))
      res)))

(defn gmaxloop
  [f max-vector]
  (loop [[x & xs] max-vector res [(last max-vector)]]
    (if x
      (if (> (f x) (f (first res))) 
        (recur xs (conj (rest res) x))
        (recur xs res))
      (first res))))

(defn max'
  [coll]
  (loop [[x & xs] coll tmp 0]
    (if x 
      (if (> tmp x)
        (recur xs tmp)
        (recur xs x))
      tmp)))

;; 

(defn gmaxloopg
  [max-vector]
  (loop [[x & xs] max-vector res [(last max-vector)]]
    (if x
      (if (> x (first res)) 
        (recur xs (conj (rest res) x))
        (recur xs res))
      (first res))))

(defn firstg
  [[x & xs]]
  (if x
    (cons (first x) (firstg xs))
    []))

(defn secondg
  [[x & xs]]
  (if x
    (cons (second x) (secondg xs))
    []))
;; max by (maxby second [[2 3] [6 7] [9 2]]) = [6 7]
(defn grange 
  [a b]
  (loop [a a b b res []]
    (if (< a b)
      (recur (inc a) b (conj res a))
      res)
    ))

;; min by (minby second [[2 3] [6 7] [9 2]]) = [9 2]
(defn gduplicate 
  [j]
  (loop [[x & xs] j res []] 
    (if x
      (recur xs (conj (conj res x) x))
      res)))

;; reduce

(defn gfibo
  [fibo]
  (loop [x fibo res [] p 1 q -1]
    (if (< x 1)
      res
    (recur (dec x) (conj res p) (+ p (nth res q 0)) (inc q)))))


(defn gyea
  [x y]
  (loop [x x y y res []]
    (if (or (empty? x) (empty? y))
    res
    (recur (rest x) (rest y) (conj res (first x) (first y))))))

;; 0! = 0
(defn gfac
  [x]
  (loop [x x a 1 b 1 c [0]]
    (if (< x 0)
      (first c)
      (recur (dec x) (* a b) (inc b) (conj (rest c) a)))))

;; 0! = 1
(defn gfactorial
  [x]
  (loop [x x a 1 b 1]
    (if (< x 1)
      a
      (recur (dec x) (*' a b) (inc b)))))

(defn gpangkat
  [a b]
  (if (< b 1)
    1
     (* a (gpangkat a (dec b)))))




(defn gpangkat2
  [x y]
  (if (< y 1)
    1
    (* x (gpangkat2 x (dec y)))))

(defn fac-recur
  [a]
  (if (< a 1)
    1
    (* a (fac-recur (dec a)))))


(defn fibo-list 
  [i]
  (nth (iterate #(conj % 
                       (+ (last %)
                          (last (butlast %))))
                [1 1]) 
       (- i 1)))

(def lfib (cons 1 (cons 2 (lazy-seq (map + lfib (rest lfib))))))

(defn max-recur
  [[x & xs]]
  (if x 
    (max x (max-recur xs))
    0))














