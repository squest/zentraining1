(ns euler.problemsolving
  (:require
    [euler.core :as core]
    [clojure.string :as cs]
    [clojure.edn :as edn]))/


;No 5
(def divisable
  (memoize (fn
             ([guess div] (divisable guess div div))
             ([guess dectest maxdiv] (if (= dectest 1)
                                       guess
                                       (if (= (mod guess dectest) 0)
                                         (recur guess (dec' dectest) maxdiv)
                                         (recur (inc' guess) maxdiv maxdiv)))))))
(defn single-divisable [guess div]
      (if (= (mod guess div) 0)
        guess
        (recur (+' guess 1) div)))

(def addexp
  (memoize (fn
             ([begin end] (addexp begin end 0))
             ([begin end ans] (cond (= begin end) (+' (core/exp begin begin) ans)
                                    ;(<= (+' begin 10) end) (+' (recur begin (+' begin 9)) (addexp (+' begin 10) end))
                                    :else (recur (+' begin 1) end (+' (core/exp begin begin) ans)))))))


;No30
(defn ispower? [a]
      (= a (reduce + (map #(core/exp % 5) (core/numtodig a)))))

(defn no30
      ([a] (no30 a 0 1000000))
      ([a ans end] (cond (> a end) ans
                         (ispower? a) (recur (inc a) (+ a ans) end)
                         :else (recur (inc a) ans end))))

;No 92 chain
(defn which92b? [n]
  (if (or (= n 89) (= n 1)) n
                            (->> (core/numtodig n)
                                 (map core/square)
                                 (reduce +)
                                 (recur))))

(defn no92b [max]
  (->> (range 1 (inc max))
       (map which92b?)
       (remove #(= 1 %))
       (count)))

(def which92?
  (memoize (fn
             ([n] (which92? 0 (core/numtodig n)))
             ([ans thelist] (if (empty? thelist) (cond (or (= ans 44) (= ans 32) (= ans 13) (= ans 10) (= ans 1)) 1
                                                       (or (= ans 85) (= ans 89) (= ans 145) (= ans 42) (= ans 20) (= ans 4) (= ans 16) (= ans 37) (= ans 58) (= ans 89)) 89
                                                       :else (recur 0 (core/numtodig ans)))
                                                 (recur (+' ans (core/square (first thelist))) (rest thelist)))))))
(defn no92
  ;(memoize (fn
  ([max] (no92 max 0))
  ([max ans] (cond  (> 1 max) ans
                    (= 89 (which92b? max)) (recur (dec' max) (inc' ans))
                    :else (recur (dec' max) ans))))