

(defn small []
  (loop [i 10000]
    (when (> i 2520)
      (if (divisible i) (println i))
      (recur (- i 1)))))

(defn divisible [x]
  (= 0 (rem x 20) (rem x 19) (rem x 18) (rem x 17) (rem x 16) (rem x 15) (rem x 14) (rem x 13) (rem x 12) (rem x 11) (rem x 10) (rem x 9) (rem x 8) (rem x 7) (rem x 6) (rem x 5) (rem x 4) (rem x 3) (rem x 2) (rem x 1)))
