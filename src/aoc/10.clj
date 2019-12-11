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
    (cond
      (and (= x1 x2) (> y2 y1)) Double/POSITIVE_INFINITY
      (and (= x1 x2) (< y2 y1)) Double/NEGATIVE_INFINITY
      :else (if (> x2 x1)
              (str "<" (/ (- y2 y1) (- x2 x1)))
              (str ">" (/ (- y2 y1) (- x2 x1)))))))

(defn negate-slope [slope]
  (cond
    (= slope Double/POSITIVE_INFINITY) Double/NEGATIVE_INFINITY
    (= slope Double/NEGATIVE_INFINITY) Double/POSITIVE_INFINITY
    :else (if (= \< (nth slope 0))
            (str ">" (drop 1 slope))
            (str "<" (drop 1 slope)))))

(def combinations (combo/combinations points 2))
(def sorted-combinations (sort-by #(:y (first %)) (sort-by #(:x (first %)) combinations)))
(def grouped-combinations (partition-by (fn [points] {:x (:x (first points))
                                                      :y (:y (first points))})
                                        sorted-combinations))
(defn calc-slopes [combinations]
  (->> combinations
       (map #(apply calc-slope %))))

(defn get-visible [combinations]
  (->> combinations
       (map #(apply calc-slope %))
       (into #{})
       (count)))

(defn create-slope-map []
  (loop [combinations combinations
         state {}]
    (let [[curr & rest] combinations
          [point1 point2] curr
          key1 (str (:x point1) "," (:y point1))
          key2 (str (:x point2) "," (:y point2))
          slope (calc-slope point1 point2)]
      (if (empty? rest)
        (-> state
            (update key1 #(conj % slope))
            (update key2 #(conj % (negate-slope slope))))
        (recur rest (-> state
                        (update key1 #(conj % slope))
                        (update key2 #(conj % (negate-slope slope)))))))))
(def slope-map (create-slope-map))

(defn solve []
  (->> slope-map
       (vals)
       (map #(into #{} %))
       (map count)
       (apply max)))


(defn calc-slope2 [point1 point2]
  (let [x1 (:x point1)
        x2 (:x point2)
        y1 (:y point1)
        y2 (:y point2)]
    (let [deltax (- x2 x1)
          deltay (- y2 y1)]
      {:x x2
       :y y2
       ;; :slope (/ deltay deltax)
       :distance (Math/sqrt (+ (Math/pow deltax 2) (Math/pow deltay 2)))
       :angle (let [rad (Math/atan2 deltax (* -1 deltay))]
                (cond
                  (and (= deltax 0) (> 0 deltay)) 0
                  :else (if (< 0 rad)
                          rad
                          (+ rad (* 2 Math/PI)))))})
    ;; (cond
    ;;   (and (= x1 x2) (> y2 y1)) {:x x2
    ;;                              :y y2
    ;;                              :slope Double/POSITIVE_INFINITY
    ;;                              :angle Math/PI}
    ;;   (and (= x1 x2) (< y2 y1)) {:x x2
    ;;                              :y y2
    ;;                              :slope Double/POSITIVE_INFINITY
    ;;                              :angle 0}
    ;;   :else (let [deltax (- x2 x1)
    ;;               deltay (- y2 y1)]
    ;;           {:x x2
    ;;             :y y2
    ;;             :slope (/ deltay deltax)
    ;;            :angle (Math/atan2 deltax deltay)}))
    ))

;; 19,11 is where the station is
(def station {:x 19 :y 11})
(def non-station
  (->> points
       (filter #(not (= % station)))))

(def grouped-points
  (->> non-station
       (map (fn [point2] (calc-slope2 station point2)))
       (sort-by (juxt :angle :distance))
       (partition-by :angle)))

(def point-list
  (->> (for [i (range 9)
             group grouped-points]
         (get (into [] group) i))
       (filterv not-empty)))

(defn illustrate [point-list asteroid-map idx]
  (if (empty? point-list)
    asteroid-map
    (let [[point & rest] point-list
          x (:x point)
          y (:y point)]
      (recur rest (assoc-in asteroid-map [y x] idx) (inc idx)))))

(def illustrated-list (illustrate point-list asteroid-map 0))

(defn solve2 []
  (get point-list 199))
