(ns basicone.arie.euler)

;#1
(reduce + (distinct (concat (range 0 1000 5) (range 0 1000 3))))

;#2
(defn fibo [lim]
  (loop [n lim
         n1 1
         n2 2
         vc [1 2]]
    (if (>= (last vc) n)
      (butlast vc)
      (recur n n2 (+ n1 n2) (conj vc (+ n1 n2))))))

(reduce + (filter even? (fibo 4000000)))

;#6
(defn square [num] (* num num))

(defn sqosm [nf nl] (reduce + (map #(square %) (range nf (+ nl 1)))))

(defn smosq [nf nl] (square (reduce + (range nf (+ nl 1)))))

(defn p3 [nf nl] (- (smosq nf nl) (sqosm nf nl)))

;#16
(defn prodig [dig] (map #(read-string (str %)) (str dig)))

(defn pow [num pow] (apply *' (take pow (repeat num))))

(apply +' (prodig (pow 2 1000)))

;#25
(defn dignum [num] (count (prodig num)))

(defn fibo2 [lim]
  (loop [n lim
         n1 1N
         n2 2N
         vc [1 1]]
    (if (= (dignum (last vc)) n)
      (+ (count vc) 1)
      (recur n n2 (+ n1 n2) (conj vc (+ n1 n2))))))
