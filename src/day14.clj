(ns day14
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.core.matrix :as m]))

(def test-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn do-steps [steps size [p v]]
  (mapv #(mod %1 %2) (m/add p (m/mul steps v)) size))

(defn coords->safety-factor [[size-x size-y] coords]
  (let [x-split (/ (dec size-x) 2)
        y-split (/ (dec size-y) 2)]
    (* (count (filter (fn [[x y]] (and (< x x-split) (< y y-split))) coords))
       (count (filter (fn [[x y]] (and (< x x-split) (> y y-split))) coords))
       (count (filter (fn [[x y]] (and (> x x-split) (> y y-split))) coords))
       (count (filter (fn [[x y]] (and (> x x-split) (< y y-split))) coords)))))

(defn coords-after-steps [input size steps]
  (->> input
       (re-seq #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
       (map rest)
       (map (fn [nums] (mapv #(Integer/parseInt %) nums)))
       (map #(partition 2 %))
       (mapv #(do-steps steps size %))))

(defn safety-factor [input size steps]
  (coords->safety-factor size (coords-after-steps input size steps)))


(assert (= 12 (safety-factor test-input [11 7] 100)))
(safety-factor (slurp (io/resource "day14.txt")) [101 103] 100)

;;; Part 2

(defn print-coords [[size-x size-y] coords]
  (str/join "\n" (map (fn [y] (apply str (map (fn [x] (if ((set coords) [x y]) \X \.)) (range size-x)))) (range size-y))))

(defn count-coords-with-diag-neighbors [coords]
  (let [coord-set (set coords)
        neighbors (fn [[x y]] [[(inc x) (inc y)] [(inc x) (dec y)] [(dec x) (inc y)] [(dec x) (dec y)]])]
    (->> coords
         (filter (fn [coord] (some coord-set (neighbors coord))))
         count)))

(defn find-easter-egg [input size max-steps]
  (let [[step] (->> (range max-steps)
                    (map (fn [step]
                           [step (count-coords-with-diag-neighbors (coords-after-steps input size step))]))
                    (sort-by second)
                    last)]
    (println (print-coords size (coords-after-steps (slurp (io/resource "day14.txt")) size step)))
    step))

(find-easter-egg (slurp (io/resource "day14.txt")) [101 103] 10000)

