(ns day2
  (:require
   [parse :as parse]
   [clojure.java.io :as io]))

(def test-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn is-safe? [row]
  (let [diffs (->> row
                   (partition 2 1)
                   (mapv #(apply - %))
                   (mapv abs))]
    (and (or (= row (sort row)) (= row (reverse (sort row))))
         (>= (apply min diffs) 1)
         (<= (apply max diffs) 3))))

;;; Part 1
(defn part1 [input]
  (->> input
       parse/parse-int-mat
       (mapv is-safe?)
       (filter true?)
       count))

(assert (= 2 (part1 test-input)))
(part1 (slurp (io/resource "day2.txt")))

;;; Part 2

(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn is-safe-rec? [row]
  (some is-safe? (map #(vec-remove % row) (range (count row)))))

(defn part2 [input]
  (->> input
       parse/parse-int-mat
       (mapv is-safe-rec?)
       (filter true?)
       count))

(assert (= 4 (part2 test-input)))
(part2 (slurp (io/resource "day2.txt")))
