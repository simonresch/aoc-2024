(ns day9
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "2333133121414131402")

;;; Part 1
(defn compact [input]
  (loop [input input
         i-left 0
         i-right (dec (count input))]
    (let [val-right (get input i-right)]
      (cond
        (<= i-right i-left) input
        (= -1 val-right) (recur input i-left (dec i-right))
        (not (= -1 (get input i-left))) (recur input (inc i-left) i-right)
        :else (recur (-> input
                         (assoc i-left val-right)
                         (assoc i-right -1))
                     i-left i-right)))))

(defn filesystem-checksum [input]
  (->> input
       str/trim
       (partition 2 2 "0")
       (map-indexed (fn [i [a b]] [[i (Integer/parseInt (str a))] [-1 (Integer/parseInt (str b))]]))
       (apply concat)
       (map (fn [[i c]] (repeat c i)))
       (apply concat)
       vec
       compact
       (map-indexed (fn [i index] (* i index)))
       (filter pos-int?)
       (apply +)))

(assert (= 1928 (filesystem-checksum test-input)))
(filesystem-checksum (slurp (io/resource "day9.txt")))

;;; Part 2

(defn insert-vec [input index val]
  (let [left (subvec input 0 index)
        right (subvec input index)]
    (vec (concat left [val] right))))

(defn insert-value [input [file-id size]]
  (let [[insert-index [_ empty-size]] (->> input
                                           (map-indexed (fn [index val]
                                                          [index val]))
                                           (filter (fn [[_ val]] (= (first val) -1)))
                                           (filter (fn [[_ [_ empty-size]]] (>= empty-size size)))
                                           first)]
    (-> input
        (assoc insert-index [file-id size])
        (insert-vec (inc insert-index) [-1 (- empty-size size)]))))

(defn compact2 [input]
  (loop [input input
         r-index (dec (count input))]
    (let [r-val (get input r-index)]
      (cond
        (neg-int? r-index) input
        (= -1 (first (get input r-index))) (recur input (dec r-index))
        :else (recur (-> input
                         (assoc r-index [-1 (second r-val)])
                         (insert-value r-val))
                     (dec r-index))))))

(defn filesystem-checksum2 [input]
  (->> input
       str/trim
       (partition 2 2 "0")
       (map-indexed (fn [i [a b]] [[i (Integer/parseInt (str a))] [-1 (Integer/parseInt (str b))]]))
       (apply concat)
       vec
       compact2
       (map (fn [[i c]] (repeat c i)))
       (apply concat)
       vec
       (map-indexed (fn [i index] (* i index)))
       (filter pos-int?)
       (apply +)))

(assert (= 2858 (filesystem-checksum2 test-input)))
(filesystem-checksum2 (slurp (io/resource "day9.txt")))
