(ns day3
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn sum-mul [input]
  (->> input
       (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)")
       (mapv (fn [[_ a b]] (* (Integer/parseInt a) (Integer/parseInt b))))
       (apply +)))

(assert (= 161 (sum-mul test-input)))
(sum-mul (slurp (io/resource "day3.txt")))

;;; Part 2
(def test-input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn sum-mul2 [input]
  (->> (str/split input #"do\(\)")
       (map #(str/split % #"don't\(\)"))
       (map first)
       (map sum-mul)
       (apply +)))

(assert (= 48 (sum-mul2 test-input2)))
(sum-mul2 (slurp (io/resource "day3.txt")))

