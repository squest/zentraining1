(ns basicone.dsapoetra.sicp.p1)

;;1.1
;;10 =>10

(+ 5 3 4)                                                   ;; =>12

(- 9 1)                                                     ;;=> 8

(/ 6 2)                                                     ;;=> 3

(+ (* 2 4) (- 4 6))                                         ;;=> 6

(def a 3)                                                   ;;=>#'<namespace>/a

(def b (+ a 1))                                             ;;;;=>#'<namespace>/a

(+ a b (* a b))                                             ;;=>19

(= a b)                                                     ;;=>false

(if (and (> b a) (< b (* a b)))
  b
  a)                                                        ;;=>4

(cond
      (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)                                             ;;=>16

(+ 2 (if (> b a) b a))                                      ;;=>6

(* (cond
         (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))                                                 ;;=>16

;;end of 1.1


;;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;;end of 1.2

;;1.3
(defn square [x] (* x x))
(defn sum-of-squares [x y] (+ (square x) (square y)))
(defn two-largest-sum-square [x y z]
  (cond
    (< x y z) (sum-of-squares y z)
    (< x z y) (sum-of-squares z y)
    (< y x z) (sum-of-squares x z)
    (< y z x) (sum-of-squares z x)
    (< z x y) (sum-of-squares x y)
    (< z y x) (sum-of-squares y x)))

;;end of 1.3

;;1.4
(defn a-plus-abs-b  [a b]
  ((if (> b 0) + -) a b))
(comment
  ((if (> b 0)       +   -)    a    b       )
  (+     a  b                                               ;;b>0
  (-     a   (* b -1))                                      ;;
   ))

;;end of 1.4
;;1.5
(def p p)
(defn test [x y]
        (if (= x 0) 0 y))                                   ;;=>0, therefore applicative order
