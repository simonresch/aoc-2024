(ns day4
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

;;; Part 1

(defn str-mat-get [mat [x y]]
  (get (get mat x) y))

(defn diag [lines neg?]
  (let [c (count (first lines))]
    (->> (range (- c) (inc c))
         (map (fn [offset]
                (->> (range c)
                     (map (fn [i] [i (+ (if neg? (- c i) i) offset)]))
                     (filter #(<= 0 (second %) (dec c))))))
         (map #(map (fn [coord] (str-mat-get lines coord)) %))
         (map #(apply str %))
         (filter not-empty))))

(defn count-xmas-in-line [line]
  (+ (count (re-seq #"XMAS" line))
     (count (re-seq #"SAMX" line))))



(defn count-xmas [input]
  (let [lines (str/split-lines input)]
    (+
   ;; horizontal
     (->> lines
          (map count-xmas-in-line)
          (apply +))
   ;; vertical
     (->> lines
          (apply map vector)
          (map #(apply str %))
          (map count-xmas-in-line)
          (apply +))
   ;; diagonal
     (->> (diag lines false)
          (map count-xmas-in-line)
          (apply +))
     (->> (diag lines true)
          (map count-xmas-in-line)
          (apply +)))))

(assert (= 18 (count-xmas test-input)))

(count-xmas (slurp (io/resource "day4.txt")))

;;; Part 2

(defn is-x-mas? [lines]
  (let [diag1 (->> [[0 0] [1 1] [2 2]]
                   (map (partial str-mat-get lines))
                   (apply str))
        diag2 (->> [[0 2] [1 1] [2 0]]
                   (map (partial str-mat-get lines))
                   (apply str))]
    (and (or (= diag1 "MAS") (= diag1 "SAM"))
         (or (= diag2 "MAS") (= diag2 "SAM")))))

(defn count-x-mas [input]
  (let [lines (str/split-lines input) ]
    (->> (for [x (range (- (count (first lines)) 2))
               y (range (- (count lines) 2))]
           (->> (range 3)
                (map #(get lines (+ y %)))
                (mapv #(subs % x (+ x 3)))))
         (filter is-x-mas?)
         count)))


(assert (= 9 (count-x-mas test-input)))

(count-x-mas (slurp (io/resource "day4.txt")))

