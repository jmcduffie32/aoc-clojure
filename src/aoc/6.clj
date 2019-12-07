(ns aoc.6
  (:require [clojure.string :as str]))

(def input-file "./resources/6_1.txt")
;; (def input-file "./resources/6_test.txt")

(defn get-orbit-map []
  (-> (slurp input-file)
      (str/split-lines)
      (->> (map #(str/split % #"\)"))
           (reduce (fn [dict orbit]
                     (assoc dict
                            (second orbit)
                            (first orbit)))
                   {}))))
(def orbit-map (get-orbit-map))
(defn get-orbits [planet orbit-list]
  (let [next-planet (get orbit-map planet)
        new-list (conj orbit-list next-planet)]
    (if (= next-planet "COM")
      new-list
      (recur next-planet new-list))))

(defn solve []
  (->> (into [] (for [[k v] orbit-map]
                 (if (not (= "COM" k))
                   (count (get-orbits k [])))))
       (apply +)))

(def you-orbits (into #{} (get-orbits "YOU" [])))
(def santa-orbits (into #{} (get-orbits "SAN" [])))

(defn solve2 []
  (-> (clojure.set/difference you-orbits santa-orbits)
      (clojure.set/union (clojure.set/difference santa-orbits you-orbits))
      (count)))
