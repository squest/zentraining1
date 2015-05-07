(ns basicone.sessthree.one)

;; ayo woi

;; Topic : higher order function

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

;; 1. re-implement reduce (reduce #(whatever %1 %2) xs)
;; 2. bikin function max-by & min-by -> ex. (max-by f xs)






