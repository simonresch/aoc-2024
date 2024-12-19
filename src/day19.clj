(ns day19
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn start-with? [v prefix]
  (and (>= (count v) (count prefix))
       (= (take (count prefix) v) prefix)))

(defn drop-prefix [v prefix]
  (drop (count prefix) v))

(def is-possible?
  (memoize (fn [towels design]
             (if (empty? design)
               true
               (let [new-designs (->> towels
                                      (filter #(start-with? design %))
                                      (map #(drop-prefix design %)))]
                 (or (some empty? new-designs)
                     (some #(is-possible? towels %) new-designs)))))))

(defn num-possible [input]
  (let [[towels designs] (str/split input #"\n\n")
        towels (mapv vec (str/split towels #", "))
        designs (mapv vec (str/split designs #"\n"))]
    (->> designs
         (map #(is-possible? towels %))
         (filter identity)
         count)))

(assert (= 6 (num-possible test-input)))
(num-possible (slurp (io/resource "day19.txt")))

;;; Part 2

(def num-arrangements
  (memoize (fn [towels design]
             (if (empty? design)
               1
               (->> towels
                    (filter #(start-with? design %))
                    (map #(drop-prefix design %))
                    (map #(num-arrangements towels %))
                    (apply +))))))

(defn total-arrangements [input]
  (let [[towels designs] (str/split input #"\n\n")
        towels (mapv vec (str/split towels #", "))
        designs (mapv vec (str/split designs #"\n"))]
    (->> designs
         (map #(num-arrangements towels %))
         (apply +))))

(assert (= 16 (total-arrangements test-input)))
(time (total-arrangements (slurp (io/resource "day19.txt"))))
