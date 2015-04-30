(ns basicone.sessone.loop1st)

;; A1. Product 
(defn new-product
  [[x & xs]]
  (if x (* x (new-product xs)) 1))

;; A2. Product Loop
(defn new-product-l
  [coll]
  (loop [[x & xs] coll res 1]
    (if x
      (recur xs (* res x))
      res )))

;; B1. Exponent
(defn new-expt
  [a m]
  (if (= m 0) 1 (* a (new-expt a (dec m)))))

;; B2. Exponent Loop
(defn new-expt-l
  [angka pangkat]
  (loop [a angka p pangkat res 1]
    (if (= p 0)
      res
      (recur a (dec p) (* res a)))))

;; C1. Count
(defn new-count
  [[x & xs]]
  (if x
    (inc (new-count xs))
    0))

;; C2. Count Loop
(defn new-count-l
  [coll]
  (loop [[x & xs] coll res 0]
    (if x
      (recur xs (+ res 1))
      res)))

;; D1. Factorial
(defn new-fact
  [i]
  (if (= i 1) 1 (* i (new-fact (dec i)))))

;; D2. Factorial Loop
(defn new-fact-l
  [i]
  (loop [n i res 1]
    (if (= n 0)
      res
      (recur (dec n) (* res n)))))

;; E1. Reverse
(defn new-reverse
  [[x & xs]]
  (if x
    (conj (new-reverse xs) x)
    []))

;; E2. Reverse Loop
(defn new-reverse-l
  [coll]
  (loop [[x & xs] coll res []]
    (if x
      (recur xs (cons x res))
      res )))

;; F1. Drop
(defn new-drop
  [n xs]
  (if (= n 0) xs (new-drop (dec n) (rest xs))))

;; F2. Drop Loop
(defn new-drop-l
  [numdrop coll]
  (loop [x numdrop res coll]
    (if (= x 0)
      res
      (recur (dec x) (rest res)))))

;; G1. Take
(defn new-take
  [n [x & xs]]
  (if (or (empty? [x]) (= n 0))
    []
    (cons x (remove nil? (new-take (dec n) xs)))))

;; G2. Take Loop
(defn new-take-l
  [numtake coll]
  (loop [i numtake [x & xs] coll res []]
    (if (= i 0)
      (remove nil? res)
    (recur (dec i) xs (conj res x)))))

; H1. Remove index
(defn new-rem-index
  [index [x & xs]]
  (cond 
    (= index 0) xs
    (empty? xs) [x]
    :else (cons x (new-rem-index (dec index) xs))))

; H2. Remove index Loop
(defn new-rem-index-l
  [index coll]
  (loop [i index [x & xs] coll res []]
    (cond 
      (= i 0) (concat res xs)
      (empty? xs) res
      :else (recur (dec i) xs (conj res x)))))

;; I1. Remove element
(defn new-rem-element
  [element [x & xs]]
  (cond 
    (empty? xs) [x]
    (= element x) (new-rem-element element xs)
    :else (cons x (new-rem-element element xs))))

;; I2. Remove element Loop
(defn new-rem-element-l
  [element coll]
  (loop [[x & xs] coll res []]
    (cond 
      (empty? xs) res
      (= element x) (recur xs res)
      :else (recur xs (conj res x)))))


;J1. Max

(defn max-2-numbers
  [a b]
  (if (>= a b) a b))

(defn new-maxi
  [[x & xs]]
  (if (empty? xs) x (max-2-numbers x (new-maxi xs))))

;J2. Max Loop
(defn new-maxi-l
  [coll]
  (loop [[x & xs] coll res x]
    (if x
      (recur xs (max-2-numbers x res))
      res)))

;K1. Min

(defn min-2-numbers
  [a b]
   (if (<= a b) a b))

(defn new-mini
  [[x & xs]]
  (if (empty? xs) x (min-2-numbers x (new-mini xs))))


;K2. Min Loop
(defn new-mini-l
  [coll]
  (loop [[x & xs] coll res x]
    (if x
      (recur xs (min-2-numbers x res))
      res )))
