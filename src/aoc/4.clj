(ns aoc.4 (:require [clojure.string :as str]))

(def range-min 307237)
(def range-max 769058)
;; 366666 possibilites

(defn increments? [number]
  (-> number
      (str)
      (str/split #"")
      (->> (mapv clojure.edn/read-string)
           (partition 2 1)
           (filter (fn [pair] (< (second pair) (first pair)))))
      (empty?)))

(defn has-a-double? [number]
  (-> number
      (str)
      (str/split #"")
      (->> (mapv clojure.edn/read-string)
           (partition 2 1)
           (filter (fn [pair] (= (second pair) (first pair)))))
      (not (partial empty?))))

(defn has-no-five-repeats? [number]
  (-> number
      (str)
      (str/split #"")
      (->> (partition 5 1)
           (some (partial apply =)))
      (not)))

(defn all-same-digits? [number]
  (-> number
      (str)
      (str/split #"")
      (->>(apply =))))

(defn not-all-same-digits? [number]
  (-> number
      (str)
      (str/split #"")
      (->>(apply =))
      (not)))


(defn solve [range-min range-max]
  (->> (range range-min range-max)
       (filter #(and
                 (has-no-five-repeats? %)
                 (not-all-same-digits? %)
                 (has-a-double? %)
                 (increments? %)))
       (count)))
