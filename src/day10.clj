(ns day10
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn get-coord [m [x y]]
  (get (get m x) y))

(defn valid-coord? [m [x y]]
  (and (<= 0 x (dec (count m)))
       (<= 0 y (dec (count (first m))))))

(defn next-coords [m [x y]]
  (let [coords [[x (inc y)]
                [(inc x) y]
                [x (dec y)]
                [(dec x) y]]]
    (filter #(valid-coord? m %) coords)))

(defn reachable-9s [input path]
  (let [curr-val (get-coord input (last path))
        next-paths (->> (next-coords input (last path))
                        (filter (fn [coord] (= (inc curr-val) (get-coord input coord))))
                        vec)]
    (cond
      (= 9 curr-val) [(last path)]
      (empty? next-paths) nil
      :else (distinct (mapcat #(reachable-9s input (conj path %)) next-paths)))))

(defn trailhead-score-sum [input]
  (let [input (->> input
                   str/split-lines
                   (mapv (fn [line] (mapv #(Integer/parseInt (str %)) line))))
        input-with-coord (for [x (range (count input))
                               y (range (count (first input)))]
                           [[x y] (get-coord input [x y])])
        trail-heads (->> input-with-coord
                         (filter (comp #{0} second))
                         (map first))]
    (->> trail-heads
         (map #(reachable-9s input [%]))
         (map count)
         (apply +))))

(assert (= 36 (trailhead-score-sum test-input)))
(trailhead-score-sum (slurp (io/resource "day10.txt")))

;;; Part 2
(defn possible-paths [input path]
  (let [curr-val (get-coord input (last path))
        next-paths (->> (next-coords input (last path))
                        (filter (fn [coord] (= (inc curr-val) (get-coord input coord))))
                        vec)]
    (cond
      (= 9 curr-val) 1
      (empty? next-paths) 0
      :else (apply + (map #(possible-paths input (conj path %)) next-paths)))))

(defn trailhead-rating-sum [input]
  (let [input (->> input
                   str/split-lines
                   (mapv (fn [line] (mapv #(Integer/parseInt (str %)) line))))
        input-with-coord (for [x (range (count input))
                               y (range (count (first input)))]
                           [[x y] (get-coord input [x y])])
        trail-heads (->> input-with-coord
                         (filter (comp #{0} second))
                         (map first))]
    (->> trail-heads
         (map #(possible-paths input [%]))
         (apply +))))

(assert (= 81 (trailhead-rating-sum test-input)))
(trailhead-rating-sum (slurp (io/resource "day10.txt")))
