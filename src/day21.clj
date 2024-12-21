(ns day21
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.core.matrix :as m]
   [clojure.math.combinatorics :as combo]))

(def test-input "029A
980A
179A
456A
379A")

(defn get-coord [m [x y]]
  (get (get m y) x))

(defn find-in-mat [m val]
  (->> (for [y (range (count m))
             x (range (count (first m)))]
         [x y])
       (filter #(= val (get-coord m %)))
       first))

(def dirs
  {:left [-1 0]
   :right [1 0]
   :up [0 -1]
   :down [0 1]})

(def num-keypad [[:7 :8 :9]
                 [:4 :5 :6]
                 [:1 :2 :3]
                 [nil :0 :A]])

(def dir-keypad [[nil :up :A]
                 [:left :down :right]])

(defn is-valid-path? [keypad start path]
  (if (empty? path)
    true
    (let [[first & rest] path
          path-coord (m/add start (first dirs))]
      (and (get-coord keypad path-coord)
           (recur keypad path-coord rest)))))

(defn shortest-paths [from to keypad]
  (let [[x1 y1] (find-in-mat keypad from)
        [x2 y2] (find-in-mat keypad to)]
    (->> (concat (repeat (max (- x1 x2) 0) :left)
                 (repeat (max (- x2 x1) 0) :right)
                 (repeat (max (- y1 y2) 0) :up)
                 (repeat (max (- y2 y1) 0) :down))
         (combo/permutations)
         (filter #(is-valid-path? keypad [x1 y1] %)))))

(def num-dir-inputs
  (memoize
   (fn [from to level]
     (if (zero? level)
       1
       (let [shortest-paths (shortest-paths from to dir-keypad)
             path-min-inputs (fn [path] (->> (concat path [:A])
                                             (cons :A)
                                             (partition 2 1)
                                             (map (fn [[from to]] (num-dir-inputs from to (dec level))))
                                             (apply +)))]
         (apply min (map path-min-inputs shortest-paths)))))))


(defn shortest-path-len [code num-dir-keypads]
  (->> (for [[from to] (partition 2 1 (cons :A code))]
         (let [shortest-paths (shortest-paths from to num-keypad)
               path-min-inputs (fn [path] (->> (concat path [:A])
                                               (cons :A)
                                               (partition 2 1)
                                               (map (fn [[from to]] (num-dir-inputs from to num-dir-keypads)))
                                               (apply +)))]
           (apply min (map path-min-inputs shortest-paths))))
       (apply +)))

(defn calc-complexity [input num-keypads]
  (->> input
       str/trim
       str/split-lines
       (mapv (fn [code] [(Integer/parseInt (str/replace code "A" ""))
                         (shortest-path-len (mapv #(keyword (str %)) code) num-keypads)]))
       (mapv (fn [[code len]] (* code len)))
       (apply +)))

(assert (= 126384 (calc-complexity test-input 2)))

(calc-complexity (slurp (io/resource "day21.txt")) 2)

;;; Part 2
(calc-complexity (slurp (io/resource "day21.txt")) 25)
