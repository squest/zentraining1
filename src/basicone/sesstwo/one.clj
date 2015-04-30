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







