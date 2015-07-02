; ASSIGNMENT 1
; Make these function
; take, drop, remove, expt, factorial, mini, maxi, product, reverse


(defn take' [x y]
  ;x is how many elements (from beginning) you want to take
  ;y is the list
  (if (= x (count y))
    y
    (take' x (butlast y))))


(defn drop' [x y]
  ;x is how many elements (from beginning) you want to drop
  ;y is the list
  (if (= x 0)
    y
    (drop' (- x 1) (rest y))))


(defn remove' [x y]
  ;x which element (what it says) you want to remove
  ;y is the list
  (if (empty? y)
    []
    (cond (= (last y) x) (remove' x (butlast y))
      :else (concat (remove' x (butlast y)) [(last y)]))))
            

(defn expt' [a n]
  ;a^n
  (if (= n 1)
    a
    (* a (expt' a (- n 1)))))


(defn factorial' [x]
    (if (= x 1)
      1
      (* x (factorial' (- x 1)))))


(defn mini' [x]
  (if (empty? (rest x))
    (last x)
    (if (< (first x) (second x))
      (cond (empty? (rest (rest x))) (first x)
        :else (mini' (concat [(first x)] (rest (rest x)))))
      (mini' (rest x)))))
  
  
(defn maxi' [x]
  (if (empty? (rest x))
    (last x)
    (if (> (first x) (second x))
      (cond (empty? (rest (rest x))) (first x)
        :else (maxi' (concat [(first x)] (rest (rest x)))))
      (maxi' (rest x)))))


(defn product' [x] ;input is in collumn
  (if (empty? x)
    1
    (* (first x) (product' (rest x)))))


(defn reverse' [x]
  (if (not (empty? x)) 
    (cons (last x) (reverse' (butlast x)))
    '()))



(defn take-rec
  ([x col] (take-rec x col []))
  ([x [a & b] ans] (if (= x 0)
                     ans
                     (recur (dec x) b (conj ans a)))))
  

(defn drop-rec
  ([x [a & b]] (if (= x 1)
                 b
                 (recur (dec x) b))))


(def x [1 2 3 4 5 6 7 8 1])
(def y {:a 3 :b 4})
(def z [[1 2] [3 4]])
(def a ["awol" "me" "you"])