(ns day13
  (:require
   [clojure.java.io :as io]
   [clojure.core.matrix :as m]))


(def test-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn solve [[av bv goal]]
  (assert (not= 0.0 (m/det [av bv])))
  ;; Solve the linear system of equations
  (let [solution (m/mmul (m/inverse (m/transpose [av bv])) goal)
        [a b] (mapv #(Math/round %) solution)]
    ;; Check if integer solution exists
    (when (= goal (m/add (m/mul a av) (m/mul b bv)))
      [a b])))

(defn min-tokens [input pos-add]
  (->> input
       (re-seq #"Button A: X\+(\d+),\s*Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")
       (map rest)
       (map (fn [nums] (mapv #(Integer/parseInt %) nums)))
       (map #(partition 2 %))
       (map (fn [[av bv goal]] [av bv (mapv #(+ % pos-add) goal)]))
       (mapv solve)
       (remove nil?)
       (map (fn [[a b]] (+ (* 3 a) b)))
       (apply +)))

;;; Part 1
(assert (= 480 (min-tokens test-input 0)))
(min-tokens (slurp (io/resource "day13.txt")) 0)

;;; Part 2
(min-tokens (slurp (io/resource "day13.txt")) 10000000000000)
