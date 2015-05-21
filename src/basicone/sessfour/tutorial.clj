(ns basicone.sessfour.tutorial)

(defn faktorial
  [n]
  (if (= n 1)
    1
    (*' n (faktorial (- n 1)))))

(defn faktorial-tail
  ([n] (faktorial-tail 1 1 n))
  ([i res n]
    (if (> i n)
      res
      (recur (+ i 1) (* res i) n))))

(defn faktorial-loop
  [n]
  (loop [i 1 res 1 m n]
    (if (> i n)
      res
      (recur (+ i 1) (* res i) m))))

(defn fibo
  [i]
  (cond (= i 1) 1
        (= i 2) 1
        :else (+ (fibo (- i 1))
                 (fibo (- i 2)))))








