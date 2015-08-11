;EULER BATCH 1

;No 3
(def factor
  ;factor of x above y value are...
  (memoize (fn
             ([x] (factor x 1 []))
             ([x y ans] (cond 
                      (> y (/ x 2)) (conj ans y)
                      (= x y) (conj ans y)
                      (= (mod x y) 0) (recur x (inc' y) (conj ans y))
                      :else (recur x (inc' y) ans))))))

(def prime?
 (memoize (fn
    ([num] (prime? num 1 1))
    ([num try slot] (cond 
                    (< num 2) false
                    (or (= num 2) (= num 3)) true
                    (> try (Math/sqrt num)) true
                    (< slot 1) false
                    :else (if (= (mod num (inc' try)) 0) (recur num (inc' try) (dec' slot))
                            (recur num (inc' try) slot)))))))

(defn no3 [x y]
  (if (and (= (mod x y) 0) (prime? y))
    y
    (recur x (dec y))))

(defn smpd [x y]
  ;smallest prime divisor of x above y
  (cond (<= x y) x
    (prime? y) (if (= (mod (/ x y) 1) 0)
                 y
                 (recur x (inc' y)))
    :else (recur x (inc' y))))


;No 4
(defn palindrome? [x]
  (if (or (= (count x) 0) (= (count x) 1))
    true
    (if (= (first x) (last x))
      (palindrome? (rest (butlast x)))
      false)))

(defn largest-palindrome [min-prod try max-prod]
  (cond (< max-prod min-prod) (str "There is none")
    (< try min-prod) (largest-palindrome min-prod (- max-prod 1) (- max-prod 1))
    (palindrome? (str (* try max-prod))) (str try " x " max-prod " = " (* try max-prod))
    :else (largest-palindrome min-prod (- try 1) max-prod)))


;No 5
(def divisable
  (memoize (fn
    ([guess div] (divisable guess div div))
    ([guess dectest maxdiv] (if (= dectest 1)
                            guess
                            (if (= (mod guess dectest) 0)
                              (recur guess (dec' dectest) maxdiv)
                              (recur (inc' guess) maxdiv maxdiv)))))))
(defn single-divisable [guess div]
  (if (= (mod guess div) 0)
      guess
      (recur (+' guess 1) div)))
(def exp
  (memoize (fn [a n]
             (cond
               (= n 0) 1
               (= n 1) a
               :else (*' (exp a (quot n 2)) (exp a (quot n 2)) (exp a (rem n 2)))))))
(def addexp 
  (memoize (fn 
             ([begin end] (addexp begin end 0))
             ([begin end ans] (cond (= begin end) (+' (exp begin begin) ans)
                                ;(<= (+' begin 10) end) (+' (recur begin (+' begin 9)) (addexp (+' begin 10) end))
                                :else (recur (+' begin 1) end (+' (exp begin begin) ans)))))))

;No 7
(def no7
  (memoize (fn
             ([n] (no7 n 1 0))
             ([n test jumlah]
                        (cond 
                          (<= n 0) (str "None")
                          (prime? test) (if (= n (inc' jumlah)) test
                                          (recur n (inc' test) (inc' jumlah)))
                          :else (recur n (inc' test) jumlah))))))


;No8
(defn ubah [a] (Integer/parseInt a))

(def soal8 (map ubah (map str (seq (slurp "soal8.txt")))))

(defn no8
  ([prob] (no8 prob 13 0))
  ([prob urut] (no8 prob urut 0))
  ([prob urut ans] (if (< (count prob) urut) ans
                     (if (< (reduce * (take urut prob)) ans) (recur (rest prob) urut ans)
                       (recur (rest prob) urut (reduce * (take urut prob)))))))

;No 9
(def square 
  (memoize (fn [x] (* x x))))
(def phyt
  ; a^2 + b^2 = c^2
  (memoize (fn
             ([jumlah] (phyt jumlah 1 1 1))
             ([jumlah a b c] (cond 
                               (= (* c c) (+ (* a a) (* b b))) (if (= (+ a b c) jumlah) (str "a=" a " b=" b " c=" c)
                                                                 (recur jumlah b b (inc' c)))
                               (= (* c c) jumlah) "None"
                               (> a b) (recur jumlah 1 (inc' b) c)
                               (> b c) (recur jumlah 1 1 (inc' c))
                               :else (recur jumlah (inc' a) b c))))))

;No 10
(def no10
  (memoize (fn
             ([n] (no10 n 0))
             ([n ans] (cond
                        (<= n 0) (str "None")
                        (= n 1) ans
                        (prime? n) (recur (dec' n) (+' ans n))
                        :else (recur (dec' n) ans))))))
           
;No 34
(defn numtodig [n]
  (cond (or (= (quot n 10) 0) (= (quot n 10) 0.0)) [n]
    :else (concat (numtodig (quot n 10)) [(mod n 10)])))

(defn factorial [x]
    (cond (= x 0) 1
      (= x 1) 1
      :else (* x (factorial (- x 1)))))

(def no34?
  (memoize (fn
             ([max] (no34? max (numtodig max) 0))
             ([max testlist testans] (if (empty? testlist) (if (= testans max) true false)
                                       (recur max (rest testlist) (+' (factorial (first testlist)) testans)))))))

(defn no34 
  ([max] (no34 max 0 ()))
  ([max ans thelist] (cond (= max 2) (str ans " " thelist)
               (= true (no34? max)) (recur (dec' max) (+' ans max) (conj thelist max))
               :else (recur (dec' max) ans thelist))))

;No 35
(defn circ 
  ([x] (if (< x 10) x (circ x 9 9 10 10)))
  ([x a a2 b b2] (if (> (quot x b) a2) (recur x (+ a2 (* a 10)) a2 (* b b2) b2)
                   (+ (* (- x (* (quot x b) b)) 10) (quot x b)))))
(def circprime?
  (memoize (fn
             ([x] (if (or (not (prime? x)) (some #(= % 0) (numtodig x))) false (circprime? x (circ x))))
             ([x test] (cond (= test x) true
                         (not (prime? test)) false
                         :else (recur x (circ test)))))))
(defn no35
  ([x] (no35 x 1 0))
  ([x test ans] (cond (= x test) ans
                  (circprime? test) (recur x (inc test) (inc ans))
                  :else (recur x (inc test) ans))))

;No42
(def tri (memoize (fn ([n] (/ (+ (* n n) n) 2)))))
(defn tri?
  ([x] (tri? x 1))
  ([x test] (cond (< x 1) false
              (> (tri test) x) false
              (= (tri test) x) true
              :else (recur x (inc test)))))

(defn lettertonum [x] (cond (= x \A) 1                         (= x \B) 2                         (= x \C) 3                         (= x \D) 4
                        (= x \E) 5                         (= x \F) 6                         (= x \G) 7                         (= x \H) 8
                        (= x \I) 9                         (= x \J) 10                         (= x \K) 11                         (= x \L) 12
                        (= x \M) 13                         (= x \N) 14                         (= x \O) 15                         (= x \P) 16
                        (= x \Q) 17                         (= x \R) 18                         (= x \S) 19                         (= x \T) 20
                        (= x \U) 21                         (= x \V) 22                         (= x \W) 23                         (= x \X) 24
                        (= x \Y) 25                         (= x \Z) 26))
(defn wordtonum 
  ([x] (wordtonum x 0))
  ([x ans] (if (empty? x) ans (recur (rest x) (+ (lettertonum (first x)) ans)))))

(def prob42 (slurp "soal42.txt"))

(defn no42
  ([prob] (no42 prob 0 0))
  ([prob word ans] (cond (empty? prob) ans
                   ;  (or (= (first prob) \,) (= (first prob) \"))  (if (tri? word) (recur (rest prob) 0 (inc ans)) (recur (rest prob) 0 ans))
                     :else (recur (rest prob) (+ word (lettertonum (first prob))) ans))))



;No 45
(def penta (memoize (fn ([n] (/ (- (* 3 n n) n) 2)))))
(def hexa (memoize (fn ([n] (- (* 2 n n) n)))))

(def no45
  (memoize (fn
    ([n] (no45 n n 1))
    ([n a b] (cond 
             (= (tri a) (penta b)) (no45 n a b 1)
            ; (< 10000 (- a n)) (str "exceed, a = " a " b = " b)
             (> (penta b) (tri a)) (recur n (inc' a) (dec' b))
             :else (recur n a (inc' b))))
    ([n a b c] (cond 
               (= (tri a) (hexa c)) (str "a = " a " b = " b " c = " c)
               ; (< 10000 (- a n)) (str "exceed, " "a = " a " b = " b)
               (> (hexa c) (tri a)) (no45 n (inc' a) b)
               :else (recur n a b (inc' c)))))))
(def no45ptest 
  (memoize (fn
    ([n] (no45ptest n n 1))
    ([n a b] (cond (= (tri a) (penta b)) (str "a = " a " b = " b)
            ; (< 10000 (- a n)) (str "exceed, a = " a " b = " b)
             (> (penta b) (tri a)) (recur n (inc' a) (dec' b))
             :else (recur n a (inc' b)))))))
(def no45htest 
  (memoize (fn
    ([n] (no45htest n n 1))
    ([n a b] (cond (= (tri a) (hexa b)) (str "a = " a " b = " b)
           ;  (< 10000 (- a n)) (str "exceed, " "a = " a " b = " b)
             (> (hexa b) (tri a)) (recur n (inc' a) (dec' b))
             :else (recur n a (inc' b)))))))

;No 50
(defn no50
  ([max] (no50 max (filter prime? (range max)) [] 0 0 0 0))
  ([max list templist temp ctemp ans c] (cond (empty? list) (if (empty? templist) (str ans " as sum of " c " primes") (recur max (rest templist) [] 0 0 ans c))
                                          (> temp max) (recur max (concat (rest templist) list) [] 0 0 ans c)
                                    
                                          (and (prime? temp) (< temp max) (>= ctemp c))
                                    (recur max (rest list) (conj templist (first list)) (+ temp (first list)) (inc ctemp) temp ctemp)
                                    :else (recur max (rest list) (conj templist (first list)) (+ temp (first list)) (inc ctemp) ans c))))
;===================================================================================================================================================(UNSOLVED)
;===================================================================================================================================================(UNSOLVED)
           
;No 37
; 3 13 23 43 53 73 83 113 173

; 7 17 37 47 67 97 137 167 197

;No 44


;No 92 chain
(def which92?
 (memoize (fn
            ([n] (which92? 0 (numtodig n)))
            ([ans thelist] (if (empty? thelist) (cond (or (= ans 44) (= ans 32) (= ans 13) (= ans 10) (= ans 1)) 1
                                                  (or (= ans 85) (= ans 89) (= ans 145) (= ans 42) (= ans 20) (= ans 4) (= ans 16) (= ans 37) (= ans 58) (= ans 89)) 89
                                                  :else (recur 0 (numtodig ans)))
                             (recur (+' ans (square (first thelist))) (rest thelist)))))))
                             
(defn no92
  ;(memoize (fn
  ([max] (no92 max 0))
  ([max ans] (cond  (> 1 max) ans
               (= 89 (which92? max)) (recur (dec' max) (inc' ans))
                    :else (recur (dec' max) ans))))

;128

;No 521
(def no521
  (memoize (fn
             ([n] (no521 2 n 0))
             ([m n] (no521 (inc' m) n 0))
             ([m n ans] (cond (> ans 1000000000) (recur m n (mod ans 1000000000))
                          (<= n m) (+' ans (smpd m 1))
                          :else (recur m (dec' n) (+' ans (smpd n 1))))))))

(def a [1 2 3])
(def r [22 33 44])

(def x [1 2 3 4 5 6 2 1])