(ns euler.4clojure)

(defn no62 [fun n] (cons n
                     (lazy-seq (no62 fun (fun n)))))

;No99
#(map read-string (map str (str (* %1 %2))))

;No107
(defn exp [a n]
  ;a^n
  (if (= n 1)
    a
    (*' a (exp a (-' n 1)))))

(defn expt [a n] (reduce * (repeat n a)))
