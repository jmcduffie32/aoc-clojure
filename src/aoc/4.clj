(ns aoc.4 (:require [clojure.string :as str]))

(def range-min 307237)
(def range-max 769058)

(defn increments? [number]
  (-> number
      (str)
      (str/split #"")
      (->> (mapv clojure.edn/read-string)
           (partition 2 1)
           (filter (fn [pair] (< (second pair) (first pair)))))
      (empty?)))

(defn get-digit-counts [number]
  (-> number
      (str)
      (str/split #"")
      (->> (partition-by identity)
           (map count))))


(defn solve [range-min range-max]
  (->> (range range-min range-max)
       (filter #(and
                 (->> (get-digit-counts %)
                      (some #{2}))
                 (increments? %)))
       (count)))
