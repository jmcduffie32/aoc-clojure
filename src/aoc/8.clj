(ns aoc.8 (:require [clojure.string :as str]
                    [clojure.edn :as edn]))

(def input-file "./resources/8_1.txt")
;; (def input-file "./resources/8_1_test.txt")
(def partition-size 150)
;; (def partition-size 6)

(defn solve []
  (-> (slurp input-file)
      (str/trim-newline)
      (str/split #"")
      (->> (map edn/read-string)
           (partition partition-size)
           (map sort)
           (map (partial partition-by identity))
           (map (fn [coll] {:0 (count (nth coll 0))
                            :1 (count (nth coll 1))
                            :2 (count (nth coll 2))}))
           (apply min-key #(get % :0 0))
           ((fn [layer-count]
              (* (get layer-count :1) (get layer-count :2))))
           )))

(defn find-by [fn coll]
  (-> (drop-while fn coll)
      (first)))

(def img-data
  (-> (slurp input-file)
      (str/trim-newline)
      (str/split #"")
      (->> (map edn/read-string)
           (partition partition-size)
           (apply mapv vector)
           (mapv (partial find-by #(= 2 %)))
           (mapv #(if (= 0 %) " " "X"))
           (partition 25)
           )))

(defn solve2 []
  (doseq [line img-data] (println line)))
