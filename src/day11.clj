(ns day11
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def test-input "125 17")

(defn apply-rules [num]
  (cond
    (zero? num) [1]
    (even? (count (str num))) (let [s (str num)
                                    new-string-size (/ (count s) 2)
                                    l (Long/parseLong (subs s 0 new-string-size))
                                    r (Long/parseLong (subs s new-string-size))]
                                [l r])
    :else [(* num 2024)]))

(def apply-rules-rec
  (memoize (fn
             [num steps]
             (if (zero? steps)
               1
               (->> (apply-rules num)
                    (map #(apply-rules-rec % (dec steps)))
                    (apply +))))))

(defn perform-steps [input num-steps]
  (->> (str/split (str/trim input) #" ")
       (mapv #(Integer/parseInt %))
       (mapv #(apply-rules-rec % num-steps))
       (apply +)))

(assert (= 55312 (perform-steps test-input 25)))

;;; Part 1
(time (perform-steps (slurp (io/resource "day11.txt")) 25))

;;; Part 2
(time (perform-steps (slurp (io/resource "day11.txt")) 75))
