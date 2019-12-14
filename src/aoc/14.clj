(ns aoc.14
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            ;; [incanter.core :as core]
            ;; [incanter.charts :as charts]
            ;; [incanter.datasets]
            ))

(def input-file "./resources/14_1_test.txt")

(defn get-name-and-quantity [el]
  (let [split-el (str/split el #" ")]
    {:name (keyword (second split-el))
     :quantity (first split-el)}))

(defn parse-formula [line]
  (let [[in out] (str/split line #"=>")
        input (map str/trim (str/split in #","))
        output (map str/trim (str/split out #","))]
    (-> (get-name-and-quantity (first output))
        (assoc :input (map get-name-and-quantity input)))))


(def formulas
  (-> input-file
      (slurp)
      (str/split-lines)
      (->> (map parse-formula)
           (reduce #(assoc %1 (:name %2) %2) {})
           )))

(def basic-elements
  (->> formulas
       (filter (fn [[k v]] (= (count (:input v)) 1)))
       (into {})))

(def fuel (get formulas :FUEL))

