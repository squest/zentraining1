(ns euler.4clojure)

(defn no62 [fun n] (cons n
                         (lazy-seq (no62 fun (fun n)))))

;No99
#(map read-string (map str (str (* %1 %2))))