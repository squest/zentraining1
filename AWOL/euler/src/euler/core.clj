(ns euler.core)

(def factor
  ;factor of x above y value are...
  (memoize (fn
             ([x] (factor x 1 []))
             ([x y ans] (cond
                          (> y (/ x 2)) (conj ans x)
                          (= x y) (conj ans x)
                          (= (mod x y) 0) (recur x (inc' y) (conj ans y))
                          :else (recur x (inc' y) ans))))))

(defn factorial [x]
      (cond (= x 0) 1
            (= x 1) 1
            :else (* x (factorial (- x 1)))))

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

(defn divisor [x]
  (->> (range 1 (inc (/ x 2)))
       (filter #(= 0 (mod x %)))))

(defn perfect? [x]
  (= (reduce + (divisor x)) x))

(defn primefactor [x] (filter prime? (factor x)))

(defn square [x] (* x x))

(def exp
  (memoize (fn [a n]
               (cond
                 (= n 0) 1
                 (= n 1) a
                 :else (*' (exp a (quot n 2)) (exp a (quot n 2)) (exp a (rem n 2)))))))
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

(def penta (memoize (fn ([n] (/ (- (* 3 n n) n) 2)))))
(def hexa (memoize (fn ([n] (- (* 2 n n) n)))))
(def tri (memoize (fn ([n] (/ (+ (* n n) n) 2)))))
(defn tri?
      ([x] (tri? x 1))
      ([x test] (cond (< x 1) false
                      (> (tri test) x) false
                      (= (tri test) x) true
                      :else (recur x (inc test)))))

(defn numtodig [n]
      (cond (or (= (quot n 10) 0) (= (quot n 10) 0.0)) [n]
            :else (conj (numtodig (quot n 10)) (mod n 10) )))

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

(defn ubah [a] (Integer/parseInt a))

(defn palindrome? [x]
  (cond (or (coll? x) (string? x))
        (if (or (= (count x) 0) (= (count x) 1))
          true
          (if (= (first x) (last x))
            (palindrome? (rest (butlast x)))
            false))
        :else (recur (numtodig x))))





;No 37
; 3 13 23 43 53 73 83 113 173

; 7 17 37 47 67 97 137 167 197

;No 44




;No 18
(def try18 (map str (filter #(not (= " " %)) (map str (seq (slurp "resources/soal18.txt"))))))

(defn soal18
      ([a] (soal18 a [] []))
      ([a tryans ans] (cond (empty? a) ans
                            (= (first a) "\n") (recur (rest a) [] (conj ans tryans))
                            :else (recur (rest (rest a)) (conj tryans (+ (* 10 (ubah (first a))) (ubah (second a)))) ans))))

;No 22
(def prob22
  (->> (slurp "resources/soal22.txt")
       (remove #(= \" %))
       (map lettertonum)))

(defn list22
      ([prob] (list22 prob [] []))
      ([prob ans temp] (cond (empty? prob) (->> (conj ans temp)
                                                (remove #(= [] %))
                                                (sort)
                                                (sort-by second)
                                                (sort-by first))
                             (= nil (first prob)) (recur (rest prob) (conj ans temp) [])
                             :else (recur (rest prob) ans (conj temp (first prob))))))

(defn sortname
      ([list] (sortname list [] 0))  ;list = (list22 prob22)

      ([list ans n] (let [a (first list)]
                         (cond (empty? list) ans
                               (> n (dec (count a))) (recur (rest list) (conj ans a) 0)
                               :else (if (some #(< % (nth a n))
                                               (map #(nth % n) (filter #(>= (count %) (inc n)) (rest list))))
                                       (sortname (cons (rest list) a) ans 0)
                                       (sortname list ans (inc n))) ))))

(defn no22
      ([prob] (no22 prob 0 0 1))
      ([prob word ans no] (cond (empty? prob) (->> ans (+ (* word no)))
                                (= (first prob) \,) (recur (rest prob) 0 (->> ans (+ (* word no))) (inc no))
                                :else (recur (rest prob) (+ word (lettertonum (first prob))) ans no))))

(defn no22b
      ([prob] (no22b (sort-by first (sort (list22 prob))) 1 0))
      ([prob no ans] (if (empty? prob) ans
                                       (recur (rest prob) (inc no)
                                              (->> (reduce + (first prob))
                                                   (* no)
                                                   (+ ans))))))


;No 26
(defn numto26 [n]
      (cond (or (= (quot n 10) 0) (= (quot n 10) 0.0)) [n]
            :else (concat (numto26 (quot n 10)) [(mod n 10)])))

(defn countrec
      ([a] (countrec a a [] (dec' (count a)) 0))
      ([stay a test chop ans] (cond (= chop 0) ans
                                    (empty? a) (if (and (every? #(= % (first test)) (butlast test)) (> (count (first test)) ans))
                                                 (recur stay stay [] (dec' chop) (count (first test)))
                                                 (recur stay stay [] (dec' chop) ans))

                                    (<= (count a) chop) (recur stay (drop chop a) (cons a test) chop ans)
                                    :else (recur stay (take (- (count a) chop) a) (cons (drop (- (count a) chop) a) test) chop ans))))


(defn no26
      ([d] (no26 0 0 d (/ 1.0 d) 1))
      ([ans countans d change kali] (cond
                                      (< d 1) (str "d " ans " rec = " countans)
                                      (not (= (mod (* (exp 10 (inc' kali)) change) 10) 0.0)) (recur ans countans d change (inc' kali))
                                      :else (if (> (countrec (numto26 (* (exp 10 kali) change))) countans)
                                              (recur d (countrec (numto26 (* (exp 10 kali) change))) (dec' d) (/ 1.0 d) 1)
                                              (recur ans countans (dec' d) (/ 1.0 d) 1)))))

(defn no26b
      ([d] (no26b d (/ 1.0 d)))
      ([d change] (cond
                    (= (mod (* 10 change) 10) 0.0) (/ change 10)
                    :else (recur d (* 10 change)))))





;128

;No 521
(defn smpd [x y]
      ;smallest prime divisor of x above y
      (cond (<= x y) x
            (prime? y) (if (= (mod (/ x y) 1) 0)
                         y
                         (recur x (inc' y)))
            :else (recur x (inc' y))))
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
