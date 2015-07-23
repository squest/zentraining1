(ns basicone.sfive.one)

(defn sieve1
  [lim]
  (let [primes (boolean-array (+ lim 1) true)
        res (atom [2])]
    (do (doseq [i (range 3 (+ lim 1) 2)
                :when (aget primes i)]
          (do (doseq [j (range (* i i) (+ lim 1) (* 2 i))]
                (aset primes j false))
              (swap! res #(conj @res i))))
        @res)))

(defn sieve
  [lim]
  (let [llim (Math/sqrt lim)
        primes (boolean-array (+ lim 1) true)]
    (loop [i (int 3) res (transient [2])]
      (if (> i lim)
        (persistent! res)
        (if (aget primes i)
          (if (<= i llim)
            (do (loop [j (* i i)]
                  (when (<= j lim)
                    (aset primes j false)
                    (recur (+ j i i))))
                (recur (+ i 2) (conj! res i)))
            (recur (+ i 2) (conj! res i)))
          (recur (+ i 2) res))))))