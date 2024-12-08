(ns day7
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

;;; Part 1
(defn possible-equation? [total curr-val nums operations]
  (if (empty? nums)
    (= total curr-val)
    (some #(possible-equation? total (% curr-val (first nums)) (rest nums) operations)
          operations)))

(defn possibly-true? [input operations]
  (let [[total nums] (str/split input #": ")
        total (Long/parseLong total)
        nums (mapv #(Long/parseLong %) (str/split nums #" "))]
    (when (possible-equation? total (first nums) (rest nums) operations)
      total)))

(defn sum-possible [input]
  (->> input
       str/split-lines
       (mapv #(possibly-true? % [+ *]))
       (filter identity)
       (apply +)))

(assert (= 3749 (sum-possible test-input)))
(sum-possible (slurp (io/resource "day7.txt")))

;;; Part 2

(defn concat-nums [a b]
  (Long/parseLong (str a b)))

(defn sum-possible2 [input]
  (->> input
       str/split-lines
       (mapv #(possibly-true? % [+ * concat-nums]))
       (filter identity)
       (apply +)))

(assert (= 11387 (sum-possible2 test-input)))
(sum-possible2 (slurp (io/resource "day7.txt")))
