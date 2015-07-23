(ns basicone.sfive.one)
;; sieve

(defn sieve1
  [^long lim]
  (let [primes (boolean-array (+ lim 1) true)
        res (atom (int 2))]
    (do (doseq [i (range 3 (+ lim 1) 2)
                :when (aget primes i)]
          (do (doseq [j (range (* i i) (+ lim 1) (* 2 i))]
                (aset primes j false))
              (swap! res #(+ % i))))
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

(defn jumlah-faktor
  [^long lim]
  (let [faks (int-array (+ lim 1) 1)
        llim (int (Math/sqrt lim))]
    (do (doseq [i (range 2 (+ llim 1))]
          (let [isqr (* i i)]
            (doseq [j (range (* 2 i) (+ lim 1) i)
                    :when (<= isqr j)]
              (if (== (* i i) j)
                (aset faks j (+ i (aget faks j)))
                (aset faks j (+ i (quot j i) (aget faks j)))))))
        (sequence
          (comp (map #(vector % (aget faks %)))
                (filter #(let [[a b] %] (> b a)))
                (map first))
          (range 2 (+ lim 1))))))

(defn sol23
  [^long lim]
  (let [tmp (int-array (jumlah-faktor lim))
        cnt (count tmp)
        abuns (boolean-array (+ lim 1) false)]
    (do (doseq [i (range cnt)
                :let [itmp (aget tmp i)]]
          (doseq [j (range i cnt)
                  :let [jtmp (aget tmp j)
                        stmp (+ itmp jtmp)]
                  :while (<= stmp lim)]
            (aset abuns stmp true)))
        (->> (range 12 (+ lim 1))
             (filter #(aget abuns %))
             (reduce +)
             (- (reduce + (range (+ lim 1))))))))

