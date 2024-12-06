(ns day6
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(def dirs [:up :right :down :left])

(defn next-dir [dir]
  (dir (->> (repeat 2 dirs)
            flatten
            (partition 2 1)
            (mapv vec)
            (into {}))))

(defn get-coord [mat [x y]]
  (get (get mat x) y))

(defn set-coord [mat [x y] val]
  (assoc mat x (assoc (get mat x) y val)))

(defn move-coord [coord direction]
  (let [[x y] coord]
    (case direction
      :up [(dec x) y]
      :down [(inc x) y]
      :left [x (dec y)]
      :right [x (inc y)])))

(defn valid-coord? [m [x y]]
  (and (<= 0 x (dec (count m)))
       (<= 0 y (dec (count (first m))))))

(defn start-direction [m]
  (let [all-chars (set (flatten m))]
    (cond
      (all-chars \>) :right
      (all-chars \<) :left
      (all-chars \^) :up
      (all-chars \V) :down)))

(defn start-coord [m]
  (->> (for [i (range (count m))
             j (range (count (first m)))]
         [i j])
       (filter (fn [coord] (#{\> \< \^ \V} (get-coord m coord))))
       first))

;; Part 1
(defn trace-path [m dir coord]
  (let [new-coord (move-coord coord dir)]
    (cond
      (not (valid-coord? m new-coord)) m
      (#{\#} (get-coord m new-coord)) (recur m (next-dir dir) coord)
      :else (recur (set-coord m new-coord \X) dir new-coord))))

(defn count-guard-path [input]
  (let [m (->> input
               str/split-lines
               (mapv vec))
        start (start-coord m)
        dir (start-direction m)
        m (set-coord m start \X)]
    (->> (trace-path m dir start)
         flatten
         (filter #{\X})
         count)))

(assert (= 41 (count-guard-path test-input)))
(count-guard-path (slurp (io/resource "day6.txt")))

;; Part 2
(defn is-loop-path? [m dir coord path]
  (let [new-coord (move-coord coord dir)]
    (cond
      (not (valid-coord? m new-coord)) false
      (contains? path [new-coord dir]) true
      (#{\#} (get-coord m new-coord)) (recur m (next-dir dir) coord path)
      :else (recur (set-coord m new-coord \X) dir new-coord (conj path [new-coord dir])))))


(defn count-possible-loop-paths [input]
  (let [m (->> input
               str/split-lines
               (mapv vec))
        start-coord (start-coord m)
        dir (start-direction m)
        m (set-coord m start-coord \X)]
    (->> (for [i (range (count m))
               j (range (count (first m)))]
           [i j])
         (filter (fn [coord] (= \. (get-coord m coord))))
         (filter (fn [coord] (is-loop-path? (set-coord m coord \#) dir start-coord #{})))
         count)))

(assert (= 6 (count-possible-loop-paths test-input)))
(println (count-possible-loop-paths (slurp (io/resource "day6.txt"))))
