(ns basicone.rec)

;; fak 0 = 1 => base case (exit point)
;; fak n = n * (fak (- n 1)) => recurrence

(defn fak
  [n]
  (if (= n 0)
    1
    (* n (fak (dec n)))))

;; (fak 5)
;; (* 5 (fak 4))
;; (* 5 (* 4 (fak 3)))
;; (* 5 (* 4 (* 3 (fak 2))))
;; (* 5 (* 4 (* 3 (* 2 (fak 1)))))
;; (* 5 (* 4 (* 3 (* 2 (* 1 (fak 0))))))
;; (* 5 (* 4 (* 3 (* 2 (* 1 1)))))
;; (* 5 (* 4 (* 3 (* 2))))
;; (* 5 (* 4 (* 6)))
;; (* 5 24)
;; 120


;; Sum of a list
;; base case => (sum []) = 0
;; recurrence => (+ (first xs) (sum (rest xs)))

(defn sum
  [coll]
  (if (empty? coll)
    0
    (+ (first coll) (sum (rest coll)))))

(defn sum'
  [[x & xs]]
  (if x (+ x (sum' xs)) 0))

;; (take' n lst)
;; base case => empty? lst = []
;; base case => n == 0 = []
;; recurrence => (cons (first lst) (take' (dec n..

(defn take'
  [n [x & xs]]
  (if x
    (if (= n 0)
      []
      (cons x (take' (dec n) xs)))
    []))

(defn take''
  [n [x & xs]]
  (cond (nil? x) []
        (= n 0) []
        :else (cons x (take'' (dec n) xs))))
















