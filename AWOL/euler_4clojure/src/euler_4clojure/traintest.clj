(ns euler-4clojure.traintest
  (:require [clojure.test :refer :all]
            [euler-4clojure.part2 :refer :all]))

(defn sqr [x]
  (* x x))

(deftest t-sqr
  (testing "Testing sqr"
    (is (= 4 (sqr 2)))
    (is (= 81 (sqr 9)))))

(deftest t-148
  (testing "Testing 148"
    (is (= 0 (no148 3 17 11)))
    (is (= 23 (no148 10 3 5)))
    (is (= 233168 (no148 1000 3 5)))
    (is (= "2333333316666668" (str (no148 100000000 3 5))))
    (is (= "110389610389889610389610"
         (str (no148 (* 10000 10000 10000) 7 11))))
    (is (= "1277732511922987429116"
           (str (no148 (* 10000 10000 10000) 757 809))))
    (is (= "4530161696788274281"
           (str (no148 (* 10000 10000 1000) 1597 3571))))))

