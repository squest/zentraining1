(ns basicone.dsapoetra.euler.p25)
;;util
(defn factors [n]
  (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn prime? [n]
  (= 2 (reduce +
               (for [i (range 1 (inc n))]
                 (if (= 0 (mod n i)) 1 0)))))

(defn notprime? [n]
  (not= 2 (reduce +
               (for [i (range 1 (inc n))]
                 (if (= 0 (mod n i)) 1 0)))))

(defn prime2? [n]
  (if (and (or (= 0 (rem n 2)) (= 0 (rem n 3)) (= 0 (rem n 5)) (= 0 (rem n 7))) (not= n 2) (not= n 3) (not= n 5) (not= n 7))
    false
    true))

(defn prime3? [n]
  (= 2 (count (factors n))))


(defn palindrome? [n]
  (= (->> n str reverse (apply str))
     (str n)))

(defn fib [a b] (cons a (lazy-seq (fib b (+' b a)))))

(defn power [x y]
  (if (= y 1)
    x (*' x (power x (dec y)))))


(def triangle-nums (map first (iterate (fn [[n m]] [(+ n m) (+ m 1)]) [1 2])))

(defn prime-factors-of [num]
  "Returns a sorted list of prime factors of num, including multiplicity."
  (let [q (Math/sqrt num)
        factor? (fn [nom den] (zero? (rem nom den)))]
    (loop [n num
           d 2
           r []]
      (cond
        (> d q) (concat r [n])
        (= n d) (concat r [n])
        (factor? n d) (recur (/ n d) d (conj r d))
        true          (recur n (inc d) r)))))

(defn num-divisors-fast [num]
  (let [freqs (reduce #(assoc %1 %2 (inc (get %1 %2 0)))
                      {} (prime-factors-of num))]
    (reduce #(* %1 (inc %2)) 1 (vals freqs))))

(defn euler-12-fast [divisors]
  (first (drop-while #(> divisors (num-divisors-fast %)) triangle-nums)))

;; End of utils

;;1
(defn kelipatan []
  (reduce + (distinct (concat (range 3 1000 3) (range 5 1000 5)))))

;;2
(defn fibfourmillion [x]
  (if (> 4000000 (apply max (take x (fib 1 2))))
    (recur (+ 1 x))
    (reduce + (filter even? (take x (fib 1 2))))))

;;3
(defn faktor-prima [x]
  (apply max (filter prime? (factors x))))



;;4
(defn largest-palindrome []
  (apply max (filter #(palindrome? %)
                     (for [x (range 100 (inc Integer/MAX_VALUE))
                           y (range 100 (inc Integer/MAX_VALUE))]
                       (* x y)))))

;;5

(defn divisible [x]
  (= 0 (rem x 20) (rem x 19) (rem x 18) (rem x 17) (rem x 16) (rem x 15) (rem x 14) (rem x 13) (rem x 12) (rem x 11) (rem x 10) (rem x 9) (rem x 8) (rem x 7) (rem x 6) (rem x 5) (rem x 4) (rem x 3) (rem x 2) (rem x 1)))

(defn small [x y]
  (if (< x y)
    (if (divisible x)
      x
      (recur (+ x 1) y))
    "tetot rangenya salah"))

;;6

(defn margin-natural [x]
  (- (power (reduce + (range 1 (+ x 1))) 2) (reduce + (map #(power % 2) (range 1 (+ x 1))))))

;;7
(defn first-n-prime [x]
  (take x (filter prime3? (range 1 Integer/MAX_VALUE))))


;;9
(defn pythagorean? [a b c]
  (if (= (+ (* a a) (* b b)) (* c c)) true false))

(defn thousand-pythagorean [x]
  (for [a (range 1 x)
        b (range a x)
        c (range (- x a b) x)
        :when (pythagorean? a b c)]
    (* a b c)))

;;10
(defn two-mi-prime []
  (reduce + (take-while #(> 2000000 %) (filter prime? (range 1 Integer/MAX_VALUE)))))

(defn max-n-prime [x y]
  (take x (filter prime? (range y Integer/MAX_VALUE))))

(defn lazy-primes-cgrande []
  (letfn [(enqueue [sieve n step]
                   (let [m (+ n step)]
                     (if (sieve m)
                       (recur sieve m step)
                       (assoc sieve m step))))
          (next-sieve [sieve n]
                      (if-let [step (sieve n)]
                        (-> sieve
                            (dissoc n)
                            (enqueue n step))
                        (enqueue sieve n (+ n n))))
          (next-primes [sieve n]
                       (if (sieve n)
                         (recur (next-sieve sieve n) (+ n 2))
                         (cons n (lazy-seq (next-primes (next-sieve sieve n) (+ n 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

;;12

(= (last ((fn first-n-prime [x]
            (take x (filter #(= 2 (reduce + (for [i (range 1 (inc %))](if (= 0 (mod % i)) 1 0)))) (range 1 Integer/MAX_VALUE)))) 100)) 541)

