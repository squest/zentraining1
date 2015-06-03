(defn fib [a b]
  "making sequence of fibonacci number started from 2 numbers"
  (cons a (lazy-seq (fib b (+' b a)))))

;;After this, is bruteforce

;;First trial
 (count (str (apply max (take 1000 (fib 1 1))))) ;;result 209

;;Second trial
 (count (str (apply max (take 10000 (fib 1 1))))) ;; result 2090

;;Third trial

(count (str (apply max (take 2000 (fib 1 1))))) ;; result 418

;;Fourth trial

 (count (str (apply max (take 8000 (fib 1 1)))));; result 1672, closer

;;Fifth trial

 (count (str (apply max (take 6000 (fib 1 1)))));; 1245

;;Sixth trial

 (count (str (apply max (take 5000 (fib 1 1))))) ;; 1045, much closer

;;Seventh trial

 (count (str (apply max (take 4000 (fib 1 1))))) ;; 836, so between 4000 and 5000

;;Eighth trial

(count (str (apply max (take 4500 (fib 1 1))))) ;; 941, darn!

;;Ninth trial

 (count (str (apply max (take 4800 (fib 1 1)))));; 1003, so close

;;tenth trial
 (count (str (apply max (take 4780 (fib 1 1))))) ;; 999

 (count (str (apply max (take 4782 (fib 1 1)))));; 1000, butjust to make sure

 (count (str (apply max (take 4781 (fib 1 1)))));; 999, so answer is 4782
