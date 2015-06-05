(ns basicone.dsapoetra.realworld.mapcycle)
;;(use 'clojure.java.io)

(defn file-to-read [] (slurp "/Users/dimassaputra/Code/project-euler/zentraining1/src/basicone/dsapoetra/realworld/mapcycle.txt/"))


(def file-to-write "/Users/dimassaputra/Code/project-euler/zentraining1/src/basicone/dsapoetra/realworld/")

(defn map-to-write [x]
  (map #()))

(defn mapparser [a]
    (map #(apply str %)
         (remove #(= (first %) \return)
                 (partition-by #(or (= \return %) (= \newline %)) a))))
(defn mapdirs [n]
  (map #(str "map" % ".txt") (range 1 (inc n))))

(defn snpmap [maps]
  (apply str (interpose "\r\n" (shuffle (mapparser maps)))))

(defn createmap [map content]
  (spit (str file-to-write map) content))

(defn shufflemap [n]
  (map #(createmap % (snpmap (file-to-read))) (mapdirs n)))
