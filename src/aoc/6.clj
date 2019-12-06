(ns aoc.6
  (:require [clojure.string :as str]))

(defn get-orbit-map []
  (-> (slurp "./resources/6_1.txt")
      (str/split #"\n")
      (->> (map #(str/split % #"\)"))
           (map (fn [val] [(keyword (first val)), (keyword (second val))]))
           (reduce (fn [dict orbit]
                     (assoc dict
                             (second orbit)
                             (first orbit)))
                   {}))))
(def orbit-map (get-orbit-map))
;; (defn solve []
;;   (->> 
;;        ))
(defn get-orbits [planet orbit-list]
  (let [next-planet (planet orbit-map)
        new-list (conj orbit-list next-planet)]
    (if (= planet :COM)
      (filter (fn [v] (not (nil? v))) new-list)
      (recur next-planet new-list))))

(defn solve []
  (into {} (for [[k v] orbit-map] {k (get-orbits k [])})))
