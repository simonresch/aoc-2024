(ns day5
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

;;; Part 1
(defn invalid-update? [rules nums]
  (->> (range (count nums))
       (map (fn [i] [(get nums i) (set (take i nums))]))
       (some (fn [[curr prev-nums]] (not-empty (set/intersection prev-nums (get rules curr)))))))

(defn update-line-num [rules update-line]
  (let [update-line (mapv #(Integer/parseInt %) (str/split update-line #","))]
    (if (invalid-update? rules update-line) 0 (get update-line (quot (count update-line) 2)))))

(defn parse-rules [rules]
  (->> rules
       str/trim
       str/split-lines
       (map #(str/split % #"\|"))
       (map (fn [[a b]] [(Integer/parseInt a) (Integer/parseInt b)]))
       (group-by first)
       (map (fn [[k v]] [k (map second v)]))
       (map (fn [[k v]] [k (set v)]))
       (into {})))

(defn sum-valid-updates [input]
  (let [[rules updates] (str/split input #"\n\n")
        rules (parse-rules rules)]
    (->> (str/trim updates)
         str/split-lines
         (map (partial update-line-num rules))
         (apply +))))

(assert (= 143 (sum-valid-updates test-input)))
(sum-valid-updates (slurp (io/resource "day5.txt")))

;;; Part 2
(defn invliad-update-line-num [rules update-line]
  (let [update-line (mapv #(Integer/parseInt %) (str/split update-line #","))
        ordered-update-line (vec (sort (fn [a b] (if (contains? (get rules a) b) -1 1)) update-line))]
    (if (invalid-update? rules update-line)
      (get ordered-update-line (quot (count ordered-update-line) 2))
      0)))

(defn sum-invalid-updates [input]
  (let [[rules updates] (str/split input #"\n\n")
        rules (parse-rules rules)]
    (->> updates
         str/trim
         str/split-lines
         (map #(invliad-update-line-num rules %))
         (apply +))))

(assert (= 123 (sum-invalid-updates test-input)))
(sum-invalid-updates (slurp (io/resource "day5.txt")))
