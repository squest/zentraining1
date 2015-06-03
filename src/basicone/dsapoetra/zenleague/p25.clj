(ns basicone.dsapoetra.zenleague.p25)

;;Utils

(defn explode-to-digits [number]
        (map #(- (int %) (int \0)) (str number)))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(defn fib [a b] (cons a (lazy-seq (fib b (+' b a)))))

(defn trim [a b c] (cons a (cons b (lazy-seq (trim c (+' b c a) (+' b c (+' b c a)))))))

(defn factors [n]
	(filter #(zero? (rem n %)) (range 1 (inc n))))

(defn prime? [n]
   (= 2 (reduce +
                (for [i (range 1 (inc n))]
                  (if (= 0 (mod n i)) 1 0)))))

(defn in?
  [seq elm]
  (some #(= elm %) seq))

(defn power [x y]
  (if (= y 1)
    x (*' x (power x (dec y)))))

(defn seq-pow [x y]
  (reverse (cons 1 (map #(power y %) (range 1 (inc x))))))

;;(seq-pow 4 4)
;;(256 64 16 4)


(defn srime? [x]
  (if (or (=  3 (count (factors x))) (= 4 (count (factors x))))
      true
      false))
;; no 3

(defn decide-biggest-value [x y]
  (if (<= y x) (recur x (* y 6)) (if (> y x) (quot y 6 ) y)))

(defn base-six [x y]
 (if (< 1 y)
   (do
    (print (quot x y))
    (recur (rem x y) (quot y 6)))
   (quot x y)))

;; no 4

(defn one-p [x]
  (apply + (map factorial (explode-to-digits x))))

(defn n-p [x]
  (if (not= 0 x)
    (+ (one-p x) (n-p (- x 1)))
    x))

;;no 5

(defn fib-n [x]
  (apply max (take  x (fib 1 1))))

;; no 6

(defn check-position [x y z]
 (take z (range x 1000 y)))

;;no 7

(defn revenue [x]
  (if (not= 0 x)
      (+ (* 15000000 x) (revenue (- x 1))) x))

(defn expense [x]
  (if (not= 0 x)
      (+ (* 75000000 x) (expense (- x 1))) x))

(defn profit [x y]
  (- (revenue x) (expense y)))

;;no 8
(defn first-hundred [x]
  (take x (filter prime? (range 1 Integer/MAX_VALUE))))

(defn smallest-div [x]
  (if (= 0 (reduce + (map #(rem x %) (first-hundred 100))))
    x (recur (+ 1 x))))

;;no 9

(defn mutlak [x]
  (apply + (range 1 x)))

;;no 10

(defn triangle [x]
  (if (not= x 1)
    (+' x (triangle (-' x 1)))
    x))
(defn triangl
  ([n] (triangl n 0))
  ([n acc] (if (not= 0 n) acc (recur (dec n) (+ n acc (- n 1) )))))

(defn gauss-tail
  ([n] (gauss-tail n 1))
  ([n acc] (if (= 1 n) acc (recur (dec n) (+ n acc)))))

(defn gauss
  ([n] (gauss n 1))
  ([n acc] (if (= 1 n) acc (recur (dec n) (+' (gauss-tail n) acc)))))

(defn tri [x]
  (if (not= x 1)
    (+' (gauss-tail x) (tri (-' x 1)))
    x))


;;no 11

(defn suma []
  (apply + (filter #(= 0 (rem % 2) (rem % 3) (rem % 5) (rem % 7)) (range 1 100000001 1))))

;;12

;;13

(defn fibo []
  (apply max (take 100 (fib 1 1))))



;;14

(comment (defn first-hundred [x]
  (take x (filter prime? (range 1 Integer/MAX_VALUE)))))

(defn under-ten-thousand [x]
  (if (< (apply max (first-hundred x)) 10000)
      (do
        (println x)
        (under-ten-thousand (+ 1 x)))
      (apply + (first-hundred (- x 1)))))
;;15
(defn sumtrim [x y]
  (+' (apply max (take x (trim 1 1 1))) (apply max (take y (trim 1 1 1)))))

;;16

(defn sumprime [x]
  (apply + (first-hundred x)))
;;(- (sumprime 2000) (sumprime 999))


;;20
(defn max-digit-fib [x]
  (if (< (count (str (apply max (take x (fib 1 1))))) 1200)
    (recur (+ x 1))
    x))
;;22

(defn srime [x y]
  (if (not= 5000 x)
    (if (or (=  3 (count (factors x))) (= 4 (count (factors x))))
      (srime (+' 1 x) (+' 1 y))
      (srime (+' 1 x) y))
    y))


(defn srime2 [x y]
  (if (not= 10000 x)
    (if (or (=  3 (count (factors x))) (= 4 (count (factors x))))
      (srime2 (+' 1 x) (+' 1 y))
      (srime2 (+' 1 x) y))
    y))


(defn srime3 [x y]
  (if (not= 7000 x)
    (if (or (=  3 (count (factors x))) (= 4 (count (factors x))))
      (srime3 (+' 1 x) (+' 1 y))
      (srime3 (+' 1 x) y))
    y))


;;23
(defn modul [x]
 (if (not= x 0)
   (+' (rem x 100) (modul (- x 1)))
   x))
;;with loop

(defn modul-loop [x]
  (loop [y x]
    (if (not= y 0)
     (do
       (+' (rem y 100)) (println y) (recur (- y 1)))
     y)))

;;25
(defn tipe-srime [x y z]
  (if (not= 0 x)
    (if (srime? x)
      (if (in? (factors x) y)
        (tipe-srime (- x 1) y (inc z))
        (tipe-srime (- x 1) y z) )
      (tipe-srime (- x 1) y z)))
  z)

(defn totop
  "too long, rethink again"
  [x y z]
  (if (not= 0 x)
    (if (srime? x)
      (if (in? (factors x) y)
        (recur (- x 1) y (inc z))
        (recur (- x 1) y z))
      (recur (dec x) y z))
    z))

;;26
(defn to-ten [x]
  (let [y (dec (count (explode-to-digits x)))]
   (reduce + (map #(* %1 %2) (explode-to-digits x) (seq-pow 4 y)))) )

;;(defn to-four)

(defn decide-biggest-value-four [x y]
  (if (<= y x) (recur x (* y 4)) (if (> y x) (quot y 4 ) y)))

(defn base-four [x y]
 (if (< 1 y)
   (do
    (print (quot x y))
    (recur (rem x y) (quot y 4)))
   (quot x y)))

(defn test2 [coll]
  (reduce *' (map #(to-ten %) coll)))
