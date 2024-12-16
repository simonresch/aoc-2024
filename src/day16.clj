(ns day16
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.core.matrix :as m]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.set :as set]))

(def test-input "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(defn get-coord [m [x y]]
  (get (get m y) x))

(defn valid-coord? [m [x y]]
  (and (<= 0 y (dec (count m)))
       (<= 0 x (dec (count (first m))))))

(defn dir->diff [dir]
  (case dir
    :up [0 -1]
    :down [0 1]
    :right [1 0]
    :left [-1 0]))

(def turn-right {:up :right
                 :down :left
                 :right :down
                 :left :up})

(def turn-left
  (set/map-invert turn-right))

(defn find-in-board [board val]
  (->> (for [y (range (count board))
             x (range (count (first board)))]
         [x y])
       (filter #(= val (get-coord board %)))
       first))

(defn lowest-score [input]
  (loop [board (->> input str/trim str/split-lines (mapv vec))
         queue (priority-map [(find-in-board board \S) :right] 0)
         visited #{}]
    (let [[[coord dir] score] (first (seq queue))]
      (cond
        (= \E (get-coord board coord)) score
        :else (let [next-moves [[[(m/add coord (dir->diff dir)) dir] (inc score)]
                                [[coord (turn-right dir)] (+ score 1000)]
                                [[coord (turn-left dir)] (+ score 1000)]]
                    next-moves (->> next-moves
                                    (filter (fn [[[coord _] _]]
                                              (and (valid-coord? board coord)
                                                   (not= (get-coord board coord) \#))))
                                    (remove #(visited (first %)))
                                    (mapv (fn [[x score]]
                                            [x (min (get queue x Integer/MAX_VALUE) score)])))]
                (when (zero? (mod score 1000))
                  (println "Score" score "Queue size" (count queue)))
                (recur board (into (dissoc queue [coord dir]) next-moves) (conj visited [coord dir])))))))

(assert (= 7036 (lowest-score test-input)))
(lowest-score (slurp (io/resource "day16.txt")))


;;; Part 2
(defn optimal-seat-count [input]
  (loop [board (->> input str/trim str/split-lines (mapv vec))
         queue (priority-map (list [(find-in-board board \S) :right]) 0)
         min-score -1
         visited #{}
         optimal-seats #{}]
    (let [[path score] (first (seq queue))
          [[coord dir]] path]
      (cond
        (empty? queue) (count optimal-seats)
        (and (pos-int? min-score) (> score min-score)) (recur board (dissoc queue path) min-score visited optimal-seats)
        :else (let [next-moves [[(conj path [(m/add coord (dir->diff dir)) dir]) (inc score)]
                                [(conj path [coord (turn-right dir)]) (+ score 1000)]
                                [(conj path [coord (turn-left dir)]) (+ score 1000)]]
                    next-paths (->> next-moves
                                    (filter (fn [[[[coord _] _] _]]
                                              (and (valid-coord? board coord)
                                                   (not= (get-coord board coord) \#))))
                                    (remove #(visited (first (first %))))
                                    (mapv (fn [[x score]]
                                            [x (min (get queue x Integer/MAX_VALUE) score)])))]
                (recur board
                       (into (dissoc queue path) next-paths)
                       (if (and (= \E (get-coord board coord)) (= -1 min-score))
                         score
                         min-score)
                       (conj visited [coord dir])
                       (if (= \E (get-coord board coord))
                         (into optimal-seats (map first path))
                         optimal-seats)))))))

(assert (= 45 (optimal-seat-count test-input)))
(time (optimal-seat-count (slurp (io/resource "day16.txt"))))
