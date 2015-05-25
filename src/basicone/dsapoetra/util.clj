(defn palindrome? [n]
  (= (->> n str reverse (apply str))
     (str n)))
