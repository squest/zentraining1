(ns basicone.sesstwo.one)


;; loop -> berulang2x dengan suatu kondisi exit

(defn square [x]
  (* x x))

(defn sqr-all
  [xs]
  (loop [nl xs res []]
    (if (empty? nl)
      res
      (recur (rest nl)
             (cons (square (first nl)) res)))))

(defn sqr-all2
  [xs]
  (loop [[l & ls] xs res []]
    (if l
      (recur ls (conj res (square l)))
      res)))

(defn myrev
  [ls]
  (loop [[x & xs] ls res []]
    (if x
      (recur xs (cons x res))
      res)))

(defn myrev2
  [ls]
  (loop [xs ls res []]
    (if (empty? xs)
      res
      (recur (butlast xs)
             (conj res (last xs))))))

(defn rev1 [xs]
  (if (empty? xs)
    []
    (cons (last xs) (rev1 (butlast xs)))))

(defn rev2 [xs]
  (if (empty? xs)
    []
    (conj (rev2 (rest xs)) (first xs))))

(defn sum-genap
  [ls]
  (loop [[x & xs] ls res 0]
    (if x
      (if (even? x)
        (recur xs (+ res x))
        (recur xs res))
      res)))

(defn sum-two
  [l1 l2]
  (loop [[x & xs] l1 [y & ys] l2 res []]
    (if x
      (recur xs ys (conj res (+ x y)))
      res)))

(defn fibo
  [lim]
  (loop [a 2 b 1 res 0]
    (if (> a lim)
      res
      (if (even? a)
       (recur (+ a b) a (+ res a))
       (recur (+ a b) a res)))))

















