(ns parse
  (:require
   [clojure.string :as str]))

(defn parse-int-mat [input]
  (->> (str/trim input)
       str/split-lines
       (mapv #(str/split % #"\s+"))
       (mapv (fn [nums] (mapv #(Integer/parseInt %) nums)))))
