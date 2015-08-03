;EULER BATCH 2 (11-30)

;No 11
(def try11 (map str (filter #(not (= " " %)) (map str (seq (slurp "soal11.txt"))))))

(defn soal11
  ([a] (soal11 a [] []))
  ([a tryans ans] (cond (empty? a) ans
                    (= (first a) "\n") (recur (rest a) [] (conj ans tryans))
                    :else (recur (rest (rest a)) (conj tryans (+ (* 10 (ubah (first a))) (ubah (second a)))) ans))))
(defn soal11V  
  ([a] (soal11V a [] []))
  ([a tryans ans] (cond (empty? (first a)) ans
                    (= (count tryans) (count a)) (recur a [] (conj ans tryans))
                    :else (recur (concat (rest a) [(rest (first a))]) (conj tryans (first (first a))) ans))))
(defn soal11DLR
  ([a] (soal11DLR a 1 0 [] []))
  ([a list templist tryans ans]  (cond (empty? a) ans
                                   (= (count tryans) list) (recur a (inc' list) 0 [] (conj ans tryans))
                                   (empty? (first a)) (recur (rest a) (dec' (count a)) 0 [] ans)
                                   :else (recur (concat (concat (take templist a) [(rest (nth a templist))]) (drop (inc' templist) a))
                                                list 
                                                (inc' templist)
                                                (conj tryans (first (nth a templist)))
                                                ans))))
(defn soal11DRL
  ([a] (soal11DRL a 1 0 [] []))
  ([a list templist tryans ans]  (cond (empty? a) ans
                                   (= (count tryans) list) (recur a (inc' list) 0 [] (conj ans tryans))
                                   (empty? (first a)) (recur (rest a) (dec' (count a)) 0 [] ans)
                                   :else (recur (concat (concat (take templist a) [(butlast (nth a templist))]) (drop (inc' templist) a))
                                                list 
                                                (inc' templist)
                                                (conj tryans (last (nth a templist)))
                                                ans))))
(defn bigstH 
  ([a] (bigstH a 4 1 0))
  ([a slot tryans ans] (cond (empty? a) ans
                         (< (count (first a)) 4) (recur (rest a) 4 1 ans)
                         (= 0 slot) (if (> tryans ans) (recur (cons (rest (first a)) (rest a)) 4 1 tryans)
                                      (recur (cons (rest (first a)) (rest a)) 4 1 ans))
                         :else (recur a (dec slot) (* tryans (nth (first a) (- slot 1))) ans))))

(def prob11H (soal11 try11))
(def prob11V (soal11V (soal11 try11)))
(def prob11DLR (soal11DLR (soal11 try11)))
(def prob11DRL (soal11DRL (soal11 try11)))

(def no11 (max (bigstH prob11H) (bigstH prob11V) (bigstH prob11DLR) (bigstH prob11DRL)))


;No 12
(def tri (memoize (fn ([n] (/ (+ (* n n) n) 2)))))

(defn countfactor
  ;factor of x above y value are...
  ([x] (countfactor x 1 0))
  ([x y ans] (if (> y (Math/sqrt x)) ans
                     (cond (= y (Math/sqrt x)) (recur x (inc' y) (inc' ans))
                       (= (mod x y) 0) (recur x (inc' y) (+' ans 2))
                       :else (recur x (inc' y) ans)))))

(defn no12
  ([n] (no12 n 1))
  ([n x] (cond
           (> (countfactor (tri x)) n) (str "Triangle number no " x " = " (tri x))
           :else (recur n (inc' x)))))


;No 13
(def soal13 (map ubah (filter #(not (= "\n" %)) (map str (seq (slurp "soal13.txt"))))))
  
(defn no13
  ([x] (no13 x 49 0 0))
  ([x urut] (no13 x urut 0 0))
  ([x urut ans dig] (cond (empty? x) ans
                      (= urut 0) (recur (rest x) 49 (+ ans (+ dig (first x))) 0)
                      :else (recur (rest x) (dec urut) ans (+ dig (* (first x) (exp 10 urut)))))))


;No 14 "Elapsed time: 142622.836724 msecs"
(def no14 
  (memoize (fn
             ([mxvl] (no14 mxvl mxvl mxvl 0 0 1))
             ([mxvl test ans cnt mxvltest path] (cond
                                          (= mxvl 0) (str ans " with " cnt " path") ;(if (> cnt 3) ans 1)
                                          (= test 1) (if (> path cnt) 
                                                       (recur (dec' mxvl) mxvl mxvltest path mxvl 1)
                                                       (recur (dec' mxvl) mxvl ans cnt mxvl 1))
                                          :else (if (even? test) (recur mxvl (/ test 2) ans cnt mxvltest (inc' path))
                                                  (recur mxvl (+ (* test 3) 1) ans cnt mxvltest (inc' path))))))))
                                                               
                                         
;No 17
(def no17
  (memoize (fn
  ([a] (cond
         (or (= a 1) (= a 2) (= a 6) (= a 10)) 3
         (or (= a 4) (= a 5) (= a 9)) 4
         (or (= a 3) (= a 7) (= a 8) (= a 40) (= a 50) (= a 60)) 5
         (or (= a 11) (= a 12) (= a 20) (= a 30) (= a 80) (= a 90)) 6
         (or (= a 15) (= a 16) (= a 70)) 7
         (or (= a 13) (= a 14) (= a 18) (= a 19)) 8
         (or (= a 17)) 9
         (or (= a 100) (= a 200) (= a 600)) 10
         (or (= a 400) (= a 500) (= a 900)) 11
         (or (= a 300) (= a 700) (= a 800)) 12
         (= a 1000) 11
         ;(> a 99) (no17 a a a 10)
         :else (+' (no17 (*' (quot a 10) 10)) (no17 (mod a 10)))))
  
  
  ([a end] (no17 a (inc' a) end 0))
  
  ([a b end ans] (cond 
                   (> end 1000) "Function only for numbers (n) 0 < n < 1001"
                   (> a end) ans
                   (= a 0) (recur b (inc' b) end ans)
                   (<= a 20) (recur b (inc' b) end (+' ans (no17 a)))
                   (> a 99) (recur (-' a (*' (quot a 100) 100)) b end  (+' ans (no17 (quot a 100))))
                   :else (recur b (inc' b) end (+' ans (no17 (*' (quot a 10) 10)) (no17 (mod a 10)))))))))


(def ansno17 (+ 
               (apply + (map no17 (range 1 100)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 100)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 200)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 300)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 400)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 500)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 600)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 700)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 800)))
               (+ (apply + (map no17 (range 1 100))) (* 99 3) (* 100 (no17 900)))
               (no17 1000)
               ))

;No19
(def no19
;input is the day (with monday as number 1), hence in this case the nput is 2
  (memoize (fn
    ([d1 m1 y1 y2] (no19 (+ d1 5) m1 y1 y2 0))
    ([d1 m1 y1 y2 ans] (cond
                               (> y1 y2) ans
                               (> m1 12) (recur d1 1 (inc' y1) y2 ans)
                               (= m1 2) (cond (= (mod y1 4) 0) (if (= (mod (+' d1 28) 7) 0)
                                                                 (recur (+' d1 28) (inc' m1) y1 y2 (inc' ans))
                                                                 (recur (+' d1 28) (inc' m1) y1 y2 ans))
                                          :else (if (= (mod (+' d1 28) 7) 0)
                                                  (recur (+' d1 28) (inc' m1) y1 y2 (inc' ans))
                                                  (recur (+' d1 28) (inc' m1) y1 y2 ans)))
                               (or (= m1 9) (= m1 4) (= m1 6) (= m1 11)) (if (= (mod (+' d1 30) 7) 0)
                                                                           (recur (+' d1 30) (inc' m1) y1 y2 (inc' ans))
                                                                           (recur (+' d1 30) (inc' m1) y1 y2 ans))
                               (or (= m1 1) (= m1 3) (= m1 5) (= m1 7) (= m1 8) (= m1 10) (= m1 12)) (if (= (mod (+' d1 31) 7) 0)
                                                                                                       (recur (+' d1 31) (inc' m1) y1 y2 (inc' ans))
                                                                                                       (recur (+' d1 31) (inc' m1) y1 y2 ans))
                               :else (recur (+' d1 31) (inc' m1) y1 y2 ans))))))


;No 24
(def soal24 [0 6])

;(defn no24
  ;([soal urut] (no24 soal urut 0 0))
  ;([soal urut test ans] (cond (= urut test) ans

;(def no24 (+ (* 2 (factorial 9)) ;the third --> 2
 ;            (* 6 (factorial 8)) ;the seventh --> 7
  ;           (* 6 (factorial 7)) ;the seventh --> 8
   ;          (* 2 (factorial 6)) ;the third --> 3
    ;         (* 5 (factorial 5)) ;the sixth --> 9
     ;        (* 1 (factorial 4)) ;the second --> 1
      ;       (* 2 (factorial 3)) ;the third --> 5
       ;      (* 2 (factorial 2)))) ;since it is the last one, take the same num which is the second, and the last possibility --> 4 and 60 --> 2783915460

                                                        
;No28
(defn no28
  ([size] (no28 size 0 1 1 4))
  ([size ans num box slot] (cond (> box size) ans
                             (= slot 4) (recur size (+' ans num) (+ num box 1) (+' box 2) 1)
                             :else (recur size (+' ans num) (+' num (dec box)) box (inc slot)))))
                             

;===================================================================================================================================================(UNSOLVED)
;===================================================================================================================================================(UNSOLVED)
           

;No 18
(def try18 (map str (filter #(not (= " " %)) (map str (seq (slurp "soal18.txt"))))))

(defn soal18
  ([a] (soal18 a [] []))
  ([a tryans ans] (cond (empty? a) ans
                    (= (first a) "\n") (recur (rest a) [] (conj ans tryans))
                    :else (recur (rest (rest a)) (conj tryans (+ (* 10 (ubah (first a))) (ubah (second a)))) ans))))

;No 26
(defn numto26 [n]
  (cond (or (= (quot n 10) 0) (= (quot n 10) 0.0)) [n]
    :else (concat (numto26 (quot n 10)) [(mod n 10)])))

(defn countrec
  ([a] (countrec a a [] (dec' (count a)) 0))
  ([stay a test chop ans] (cond (= chop 0) ans
                            (empty? a) (if (and (every? #(= % (first test)) (butlast test)) (> (count (first test)) ans))
                                           (recur stay stay [] (dec' chop) (count (first test)))
                                           (recur stay stay [] (dec' chop) ans))
                         
                            (<= (count a) chop) (recur stay (drop chop a) (cons a test) chop ans)
                            :else (recur stay (take (- (count a) chop) a) (cons (drop (- (count a) chop) a) test) chop ans))))


(defn no26
  ([d] (no26 0 0 d (/ 1.0 d) 1))
  ([ans countans d change kali] (cond 
                             (< d 1) (str "d " ans " rec = " countans)
                             (not (= (mod (* (exp 10 (inc' kali)) change) 10) 0.0)) (recur ans countans d change (inc' kali))
                                    :else (if (> (countrec (numto26 (* (exp 10 kali) change))) countans)
                                            (recur d (countrec (numto26 (* (exp 10 kali) change))) (dec' d) (/ 1.0 d) 1)
                                            (recur ans countans (dec' d) (/ 1.0 d) 1)))))

(defn no26b
  ([d] (no26b d (/ 1.0 d)))
  ([d change] (cond 
                (= (mod (* 10 change) 10) 0.0) (/ change 10)
                :else (recur d (* 10 change)))))                     