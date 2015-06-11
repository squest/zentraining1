(ns basicone.arie.euler)

;#globalfunc
(declare pisdig)
(declare dignum)
(declare prime?)
(declare nextprime)

;#sabdaquest
(defn mapcycle [] (slurp "G:/Arie/Games/Counter-Strike 1.6/cstrike/mapcycle.txt"))
(def mapdir "G:/Arie/Games/Counter-strike 1.6/cstrike/mapcycle/")
(defn mapparser [a] 
    (map #(apply str %) 
         (remove #(or (= (first %) \return) (= (first %) \newline)) 
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

;#3
(defn primefactor? [div n]
  (and (prime? div) (= 0 (rem n div))))

(defn nextprimefactor [div n]
  (loop [i div
         j n]
    (if (primefactor? i j)
      i
      (recur (nextprime (inc i)) j))))

(defn maxprime [n]
  (loop [i (nextprimefactor 3 n)
         j n]
    (if (prime? j)
      j
      (recur (nextprimefactor (+ i 2) j) (/ j i)))))

;#4
(defn firsthalf [st] (take (/ (count st) 2) st))

(defn resthalf [st] (drop (/ (count st) 2) st))

(defn palin? [num] (= (firsthalf (str num)) (reverse (resthalf (str num)))))

(defn carte [ls1 ls2] (for [i (sort ls1) j (sort ls2)] [i j]))

(defn mulcarte [ls1 ls2] (map #(apply * %) (carte ls1 ls2)))

(defn maxpalin [ls1 ls2] (apply max (filter palin? (mulcarte ls1 ls2))))

;#5
(defn factors [n coll]
  (every? true? (map #(= (rem n %) 0) coll)))

(defn p5 [coll]
  (loop [a false
         b 2520]
    (if (true? a)
      (dec b)
      (recur (factors b coll) (inc b)))))

;#6
(defn square [num] (* num num))

(defn sqosm [nf nl] (reduce + (map #(square %) (range nf (+ nl 1)))))

(defn smosq [nf nl] (square (reduce + (range nf (+ nl 1)))))

(defn p3 [nf nl] (- (smosq nf nl) (sqosm nf nl)))

;#7
(defn allfactor [n]
    (filter #(= (rem n %) 0) (range 1 (inc n))))

(defn prime? [n]
  (cond
    (even? n) false
    (= (rem n 3) 0) false
    (not-any? #(= 0 (rem n %)) (range 3 (inc (Math/sqrt n)) 2)) true))

(defn nextprime [n]
    (if (prime? n) 
      n
      (recur (inc n))))

(defn p07 [n]
  (loop [i n
         j 2]
    (if (= 1 i)
      j
      (recur (dec i) (nextprime (inc j))))))

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

;#9
(defn phyta [[c b a]]
  (and (< a b c) (= (+ (square a) (square b)) (square c)) (= 1000 (+ a b c))))

(defn pfor [n]
  (for [n1 (range 1 (inc n))
        n3 (range 1 n1)
        n2 (range (inc n3) n1)]
    [n1 n2 n3]))

(defn p9 [] (reduce * (first (filter #(phyta %) (pfor 1000)))))

;#8
(defn danum [] (apply str 
                      (remove #(= \newline %) 
                              (slurp "src/basicone/arie/danum.txt"))))

(defn multstring [st] (reduce * (prodig (apply str (take 13 st)))))

(defn p8 [num] 
  (apply max (loop [n num
                    vc []]
               (if (<= (count n) 13)
                 vc
                 (recur (apply str (rest n)) (conj vc (multstring n)))))))

(p8 (danum))

;#10
(defn primesum [n]
  (loop [j 3
         val 2]
    (if (>= j n)
      val
      (recur (nextprime (+ j 2)) (+ val j)))))


;#11
(defn num11 []
  (mapparser (slurp "src/basicone/arie/p11.txt")))

(defn fouradj [nums]
  (apply max (loop [mx []
                    vc nums]
               (if (<= (count vc) 3)
                 mx
                 (recur (cons (apply * (take 4 vc)) mx) (rest vc))))))

(defn parsenum [st]
  (map #(apply str %) 
       (remove #(= \space (first %)) 
               (partition-by #(= \space %) st))))

(defn maxadj [nums]
  (fouradj (map #(Integer/parseInt %) (parsenum nums))))

(defn mapmaxadj []
  (apply max (map maxadj (num11))))

(defn parsednum []
  (map parsenum (num11)))

(defn down [nums]
  (map #(first %) nums))

(defn mapdown [xs]
  (loop [vc []
         nums xs]
    (if (some empty? nums)
      vc
      (recur (cons (apply vector (down nums)) vc) (map rest nums)))))

(defn maxdown [nums]
  (fouradj (map #(Integer/parseInt %) nums)))

(defn mapmaxdown []
  (apply max (map maxdown (mapdown (parsednum)))))

(defn diag [nums]
  (map #(nth %1 %2 nil) nums (range 0 20)))

(defn mapdiagright [xs]
  (loop [vc []
         nums xs]
    (if (some empty? nums)
      vc
      (recur (cons (apply vector (diag nums)) vc) (map rest nums)))))

(defn filtdiagright []
  (filter #(<= 4 (count %)) 
          (map #(remove nil? %) (mapdiagright (parsednum)))))

(defn filtdiagrightbtm []
  (filter #(<= 4 (count %)) 
          (map #(remove nil? %) (mapdiagright 
                                  (map reverse (reverse (parsednum)))))))

(defn parsedmapnum [nums]
  (map #(Integer/parseInt %) nums))

(defn maxdiagrighttop []
  (apply max (map fouradj (map parsedmapnum (filtdiagright)))))

(defn maxdiagrightbtm []
  (apply max (map fouradj (map parsedmapnum (filtdiagrightbtm)))))

(defn maxdiagright []
  (max (maxdiagrightbtm) (maxdiagrighttop)))

(defn filtdiagleft []
  (filter #(<= 4 (count %)) 
          (map #(remove nil? %) 
               (mapdiagright (map reverse (parsednum))))))

(defn filtdiagleftbtm []
  (filter #(<= 4 (count %)) 
          (map #(remove nil? %) 
               (mapdiagright (reverse (parsednum))))))

(defn maxdiaglefttop []
  (apply max (map fouradj (map parsedmapnum (filtdiagleft)))))

(defn maxdiagleftbtm []
  (apply max (map fouradj (map parsedmapnum (filtdiagleftbtm)))))


(defn maxdiagleft []
  (max (maxdiaglefttop) (maxdiagleftbtm)))

(defn maxmap []
  (max (maxdiagleft) (maxdiagright) (mapmaxadj) (mapmaxdown)))

;#13
(def p13num (slurp "src/basicone/arie/p13.txt"))

(defn p13 []
  (apply str (take 10 (str (reduce + (map read-string (mapparser (p13num))))))))

;#17
(defn numlet [n]
  (cond
    (= n 1) "one"
    (= n 2) "two"
    (= n 3) "three"
    (= n 4) "four"
    (= n 5) "five"
    (= n 6) "six"
    (= n 7) "seven"
    (= n 8) "eight"
    (= n 9) "nine"
    (= n 10) "ten"
    (= n 11) "eleven"
    (= n 12) "twelve"
    (= n 13) "thirteen"
    (= n 14) "fourteen"
    (= n 15) "fifteen"
    (= n 16) "sixteen"
    (= n 17) "seventeen"
    (= n 18) "eighteen"
    (= n 19) "nineteen"
    (= n 20) "twenty"
    (= n 30) "thirty"
    (= n 40) "forty"
    (= n 50) "fifty"
    (= n 60) "sixty"
    (= n 70) "seventy"
    (= n 80) "eighty"
    (= n 90) "ninety"
    (= n 100) "hundred"
    (= n 1000) "thousand"))

(defn jbrsingle [n]
  (reverse (map * (reverse (pisdig n)) [1 10 100 1000])))

(defn jbrrest [n]
  (let [stnrest (apply str (rest (str n)))
        nrest (Integer/parseInt stnrest)]
    (cond
      (= nrest 00) ""
      (= nrest 11) '(10 11)
      (= nrest 12) '(10 12)
      (= nrest 13) '(10 13)
      (= nrest 14) '(10 14)
      (= nrest 15) '(10 15)
      (= nrest 16) '(10 16)
      (= nrest 17) '(10 17)
      (= nrest 18) '(10 18)
      (= nrest 19) '(10 19)
      (= nrest 20) '(10 20)
      (= stnrest "08") '(10 8)
      (= stnrest "09") '(10 9)
      :else (list 10 (jbrsingle nrest)))))

(defn jabar [n]
  (cond
    (= (dignum n) 3) (cons (first (pisdig n)) (flatten (vector 100 (jbrrest n))))
    (= (dignum n) 4) (cons (first (pisdig n)) (flatten (vector 1000)))
    :else (jbrsingle n)))

(defn letsum [n]
  (cond 
    (< n 21) (numlet n)
    (= n 30) (numlet n)
    (= n 40) (numlet n)
    (= n 50) (numlet n)
    (= n 60) (numlet n)
    (= n 70) (numlet n)
    (= n 80) (numlet n)
    (= n 90) (numlet n)
    :else (map numlet (jabar n))))

(defn cletsum [n]
  (count (apply str (letsum n))))

;#20

(defn factorial [n]
  (loop [a n
         vc [1]]
    (if (= a 1)
      vc
      (recur (dec a) (cons a vc)))))

(defn rnf [n]
  (reduce * (factorial n)))

(defn pisdig [n]
  (map read-string (map str (str n))))

(defn p20 [] (reduce + (pisdig (rnf 100N))))

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

;#28
(defn spiralvar [n]
  (sort (flatten (take 4 (repeat (range 2 (inc n) 2))))))

(defn diagspi [n]
  (loop [vc [1]
         pv (spiralvar n)]
    (if (empty? pv)
      vc
      (recur (cons (+ (first vc) (first pv)) vc) (rest pv)))))

(defn p28 []
  (reduce + (diagspi 1001)))

;#48
(defn selfpow [num]
  (apply * (take num (repeat num))))

(defn p48 [nums]
  (apply str (take-last 10 (str (apply + (map selfpow (range 1N nums)))))))