
(defn largest-palindrome []
  (apply max (filter #(palindrome? %)
                     (for [x (range 100 (inc 999))
                           y (range 100 (inc 999))]
                       (* x y)))))
