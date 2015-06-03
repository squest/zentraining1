(ns basicone.arie.euler)

;#sabdaquest
(defn mapcycle [] (slurp "G:/Arie/Games/Counter-Strike 1.6/cstrike/mapcycle.txt"))
(def mapdir "G:/Arie/Games/Counter-strike 1.6/cstrike/mapcycle/")
(defn mapparser [a] 
    (map #(apply str %) 
         (remove #(= (first %) \return) 
                 (partition-by #(or (= \return %) (= \newline %)) a))))
(defn mapdirs [n]
  (map #(str "map" % ".txt") (range 1 (inc n))))

(defn snpmap [maps] 
  (apply str (interpose "\r\n" (shuffle (mapparser maps)))))

(defn createmap [map content]
  (spit (str mapdir map) content))

(defn shufflemap [n]
  (map #(createmap % (snpmap (mapcycle))) (mapdirs n)))

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

;#4
(defn firsthalf [st] (take (/ (count st) 2) st))

(defn resthalf [st] (drop (/ (count st) 2) st))

(defn palin? [num] (= (firsthalf (str num)) (reverse (resthalf (str num)))))

(defn carte [ls1 ls2] (for [i (sort ls1) j (sort ls2)] [i j]))

(defn mulcarte [ls1 ls2] (map #(apply * %) (carte ls1 ls2)))

(defn maxpalin [ls1 ls2] (apply max (filter palin? (mulcarte ls1 ls2))))


;#6
(defn square [num] (* num num))

(defn sqosm [nf nl] (reduce + (map #(square %) (range nf (+ nl 1)))))

(defn smosq [nf nl] (square (reduce + (range nf (+ nl 1)))))

(defn p3 [nf nl] (- (smosq nf nl) (sqosm nf nl)))

;#14
(defn collatz [sn]
  (loop [n sn
         vc []]
    (if (= n 1)
      (conj vc 1)
      (recur (cond 
               (even? n) (/ n 2)
               (odd? n) (+ (* n 3) 1))
             (conj vc n)))))

(defn countlatz [n] (count (collatz n)))

(defn findmax []
  (apply max (map countlatz (range 1 1000001))))

(defn p14 []
  (apply first (filter #(= 525 (count %)) (map collatz (range 1 1000001)))))

;#16
(defn prodig [dig] (map #(read-string (str %)) (str dig)))

(defn pow [num pow] (apply *' (take pow (repeat num))))

(apply +' (prodig (pow 2 1000)))

;#8
(def danum "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defn multstring [st] (reduce * (prodig (apply str (take 13 st)))))

(defn p8 [num] 
  (apply max (loop [n num
                    vc []]
               (if (<= (count n) 13)
                 vc
                 (recur (apply str (rest n)) (conj vc (multstring n)))))))

(p8 danum)

;#22
(defn names [] (sort (read-string (slurp "src/basicone/arie/names.txt"))))

(defn namescore [name] (apply +' (map #(- (int %) 64) name)))

(apply +' (map * (map namescore (names)) (range 1 (inc (count (names))))))

;#24 
(defn lexoo [vr vc]
  (for [a vc
        b vc
        c vc]
    [a b c]))


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
