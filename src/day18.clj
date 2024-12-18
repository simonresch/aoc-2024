(ns day18
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.data.priority-map :refer [priority-map]]))

(def test-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defn valid-coord? [size [x y]]
  (and (<= 0 y (dec size))
       (<= 0 x (dec size))))

(defn next-coords [size [x y]]
  (let [coords [[x (inc y)]
                [(inc x) y]
                [x (dec y)]
                [(dec x) y]]]
    (filter #(valid-coord? size %) coords)))


(defn parse-blocked [input num-blocked]
  (->> input
       str/trim
       str/split-lines
       (map #(str/split % #","))
       (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]))
       (take num-blocked)
       set))

(defn count-min-moves [input board-size num-blocked]
  (let [blocked (parse-blocked input num-blocked)
        goal [(dec board-size) (dec board-size)]]
    (loop [visited #{}
           queue (priority-map [0 0] 0)]
      (let [[coord score] (first (seq queue))]
        (cond
          (empty? queue) :no-path
          (= goal coord) score
          (contains? visited coord) (recur visited (dissoc queue coord))
          :else (recur (conj visited coord)
                       (->> (next-coords board-size coord)
                            (remove blocked)
                            (remove visited)
                            (map (fn [coord] [coord (inc score)]))
                            (into queue))))))))

(assert (= 22 (count-min-moves test-input 7 12)))
(count-min-moves (slurp (io/resource "day18.txt")) 71 1024)

;;; Part 2
(defn blocking-coord [input board-size start-index]
  (let [blocked (->> input
                     str/trim
                     str/split-lines
                     (map #(str/split % #","))
                     (mapv (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)])))]
    (->> (range start-index (inc (count blocked)))
         (map (fn [i] [(get blocked (dec i)) (count-min-moves input board-size i)]))
         (filter #(= :no-path (second %)))
         first
         first)))

(assert (= [6 1] (blocking-coord test-input 7 0)))
(blocking-coord (slurp (io/resource "day18.txt")) 71 1024)
