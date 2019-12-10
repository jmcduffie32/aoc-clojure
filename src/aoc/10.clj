(ns aoc.10 (:require [clojure.math.combinatorics :as combo]
                     [clojure.string :as str]))

(def input-file "./resources/10_1.txt")
;; (def input-file "./resources/10_test.txt")

(def asteroid-map
  (-> input-file
      (slurp)
      (str/split-lines)
      (->> (mapv #(str/split % #"")))))

(def x-dim (count (first asteroid-map)))
(def y-dim (count asteroid-map))

(def points
  (->> (for [y (range y-dim)
             x (range x-dim)]
         (if (= (nth (nth asteroid-map y) x) "#")
           {:x x :y y}))
       (filter not-empty)))

(defn calc-slope [point1 point2]
  (let [x1 (:x point1)
        x2 (:x point2)
        y1 (:y point1)
        y2 (:y point2)]
    (if (= x1 x2)
      :inf
      (/ (- y2 y1) (- x2 x1)))))

(def combinations (combo/combinations points 2))
(def sorted-combinations (sort-by #(:y (first %)) (sort-by #(:x (first %)) combinations)))
(def grouped-combinations (partition-by (fn [points] {:x (:x (first points))
                                                      :y (:y (first points))})
                                        sorted-combinations))
(defn calc-slopes [combinations]
  (map #(apply calc-slope %) combinations))
(defn solve []
  (->> grouped-combinations
       (map get-visible)
       (apply max)))
