
(ns day8
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(defn get-coord [m [x y]]
  (get (get m x) y))

(defn valid-coord? [m [x y]]
  (and (<= 0 x (dec (count m)))
       (<= 0 y (dec (count (first m))))))

(defn vec+ [a b]
  (mapv + a b))

(defn vec- [a b]
  (mapv - a b))

(defn vec* [f v]
  (mapv #(* f %) v))

;;; Part 1
(defn antinode-locs [m coords]
  (->> (for [l coords
             r coords]
         (when (not= l r)
           (let [diff (vec- r l)]
             [(vec+ r diff) (vec- l diff)])))
       (apply concat)
       (filter #(valid-coord? m %))))

(defn num-antinodes [input possible-antinode-locs]
  (let [m (->> input
               str/split-lines
               (mapv vec))
        m-with-coord (for [i (range (count m))
                           j (range (count (first m)))]
                       [(get-coord m [i j]) [i j]])
        grouped-coords (dissoc (group-by first m-with-coord) \.)]
    (->> grouped-coords
         (map (fn [[k vals]] (map second vals)))
         (map #(possible-antinode-locs m %))
         (apply concat)
         set
         count)))

(assert (= 14 (num-antinodes test-input antinode-locs)))
(num-antinodes (slurp (io/resource "day8.txt")) antinode-locs)

;;; Part 2

(defn antinode-locs-with-resonance [m coords]
  (->> (for [l coords
             r coords]
         (when (not= l r)
           (let [diff (vec- r l)]
             (map #(vec+ l (vec* % diff)) (range (- (count m)) (count m))))))
       (apply concat)
       (filter #(valid-coord? m %))))

(assert (= 34 (num-antinodes test-input antinode-locs-with-resonance)))
(num-antinodes (slurp (io/resource "day8.txt")) antinode-locs-with-resonance)
