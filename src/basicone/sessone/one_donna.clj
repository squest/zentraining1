(ns basicone.sessone.one-donna)

(defn -main
	[]
	(println "Hello, World!"))

(defn new-range
	"This takes two integers and returns the list of integers between a & b"
	[a b]
	(if (= a (dec b))
		[a]
		(cons a (new-range (inc a) b))))

(defn repli
	[coll time]
	(if (= time 1)
		coll
		(sort (concat coll (repli coll (dec time))))))

(defn new-product
	[[x & xs]]
	(if x
		(* x (new-product xs))
		1))

(defn new-take
	[n [x & xs]]
	(if (or (empty? xs) (= n 1))
		(cons x [])
		(cons x (new-take (dec n) xs))))

(defn new-drop
	[n xs]
	(if (= n 0)
		xs
		(new-drop (dec n) (rest xs))))

(defn new-expt
	[a m]
	(if (= m 0)
		1
		(* a (new-expt a (dec m)))))

(defn new-fact
	[i]
	(if (= i 1)
		1
		(* i (new-fact (dec i)))))

(defn new-rem
	[element [x & xs]]
	(if (or (empty? xs) (= element 0))
		xs
		(cons x (new-rem (dec element) xs))))

(defn new-reverse
	[[x & xs]]
	(if (empty? xs)
		[x]
		(conj (new-reverse xs) x)))

(defn sum-last-2
	([] (sum-last-2 1 2))
	([n m] (cons n (lazy-seq (sum-last-2 m (+ n m))))))

(defn max-2-numbers
	[a b]
	(if (>= a b)
		a
		b))

(defn min-2-numbers
	[a b]
	(if (<= a b)
		a
		b))


(defn new-maxi
	[[x & xs]]
	(if (empty? xs)
		x
		(max-2-numbers x (new-maxi xs))))

(defn new-mini
	[[x & xs]]
	(if (empty? xs)
		x
		(min-2-numbers x (new-mini xs))))

(defn new-count
	[[x & xs]]
	(if (empty? xs)
		1
		(inc (new-count xs))))
