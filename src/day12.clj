(ns day12
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

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

;;; Part 1
(defn area-and-perimiter [m start-coord]
  (loop [visited #{}
         area 0
         perimiter 0
         queue [start-coord]]
    (let [coord (first queue)
          neighbors (->> (next-coords m (or coord [-1 -1]))
                         (filter #(= (get-coord m coord) (get-coord m %))))
          perimiter-add (- 4 (count neighbors))]
      (cond
        (empty? queue) [visited area perimiter]
        (visited coord) (recur visited
                               area
                               perimiter
                               (rest queue))
        :else (recur (conj visited coord)
                     (inc area)
                     (+ perimiter perimiter-add)
                     (into (rest queue) neighbors))))))


(defn total-price [input]
  (let [m (->> input
               str/trim
               str/split-lines
               (mapv vec))
        coords (set (for [x (range (count m))
                          y (range (count (first m)))]
                      [x y]))]
    (->> coords
         (map #(area-and-perimiter m %))
         (group-by first)
         (map (comp first second))
         (map (fn [[_ area perimiter]] (* area perimiter)))
         (apply +))))

(assert (= 1930 (total-price test-input)))
(time (total-price (slurp (io/resource "day12.txt"))))

;;; Part 2
(defn num-continuous-segments [nums]
  (->> nums
       sort
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       (filter #(> % 1))
       count
       inc))

(defn fences->side-count [fences]
  (let [x-fences (->> fences
                      (filter #(int? (second %)))
                      (group-by first)
                      (mapv (fn [[_ f]]
                              (->> f
                                   (map second)
                                   (num-continuous-segments))))
                      (apply +))
        y-fences (->> fences
                      (filter #(int? (first %)))
                      (group-by second)
                      (mapv (fn [[_ f]]
                              (->> f
                                   (map first)
                                   (num-continuous-segments))))
                      (apply +))]
    (+ x-fences y-fences)))

(defn area-and-sides [m start-coord]
  (loop [visited #{}
         area 0
         fences #{}
         queue [start-coord]]
    (let [coord (first queue)
          neighbors (->> (next-coords m (or coord [-1 -1]))
                         (filter #(= (get-coord m coord) (get-coord m %))))
          [x y] coord]
      (cond
        (empty? queue) [visited area (fences->side-count fences)]
        (visited coord) (recur visited
                               area
                               fences
                               (rest queue))
        :else (recur (conj visited coord)
                     (inc area)
                     (->> [[x (inc y)] [(inc x) y] [x (dec y)] [(dec x) y]]
                          (filter #(or (not (valid-coord? m %)) (not= (get-coord m %) (get-coord m coord))))
                          (map (fn [[side-x side-y]]
                                 (if (= side-x x)
                                   [x (+ (- y (* 0.1 (- y side-y))) (if (< y side-y) 1 0))]
                                   [(+ (- x (* 0.1 (- x side-x))) (if < x side-x) 1 0) y])))
                          (into fences))
                     (into (rest queue) neighbors))))))


(defn total-price-reduced [input]
  (let [m (->> input
               str/trim
               str/split-lines
               (mapv vec))
        coords (set (for [x (range (count m))
                          y (range (count (first m)))]
                      [x y]))]
    (->> coords
         (map #(area-and-sides m %))
         (group-by first)
         (map (comp first second))
         (map (fn [[_ area perimiter]] (* area perimiter)))
         (apply +))))

(assert (= 1206 (total-price-reduced test-input)))
(time (total-price-reduced (slurp (io/resource "day12.txt"))))
