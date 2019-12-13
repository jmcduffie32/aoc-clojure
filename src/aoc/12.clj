(ns aoc.12
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]))

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

(def initial-moons
  [{:x 7 :y 10 :z 17
    :vx 0 :vy 0 :vz 0
    :name 1}
   {:x -2 :y 7 :z 0
    :vx 0 :vy 0 :vz 0
    :name 2}
   {:x 12 :y 5 :z 12
    :vx 0 :vy 0 :vz 0
    :name 3}
   {:x 5 :y -8 :z 6
    :vx 0 :vy 0 :vz 0
    :name 4}])

;; (def initial-moons
;;   [{:x -8 :y -10 :z 0
;;     :vx 0 :vy 0 :vz 0
;;     :name 1}
;;    {:x 5 :y 5 :z 10
;;     :vx 0 :vy 0 :vz 0
;;     :name 2}
;;    {:x 2 :y -7 :z 3
;;     :vx 0 :vy 0 :vz 0
;;     :name 3}
;;    {:x 9 :y -8 :z -3
;;     :vx 0 :vy 0 :vz 0
;;     :name 4}])

(defn get-velocity-update [pos1 pos2]
  (if (= pos1 pos2) 0
      (if (> pos2 pos1) 1 -1)))

(defn update-moon-velocity [moon1 moon2]
  (-> moon1
      (update :vx #(+ % (get-velocity-update (:x moon1) (:x moon2))))
      (update :vy #(+ % (get-velocity-update (:y moon1) (:y moon2))))
      (update :vz #(+ % (get-velocity-update (:z moon1) (:z moon2))))))

(defn update-velocities [moons]
  (for [[moon & others] (rotations moons)]
    (-> moon
        (update-moon-velocity (nth others 0))
        (update-moon-velocity (nth others 1))
        (update-moon-velocity (nth others 2)))))

(defn update-positions [moons]
  (for [moon moons]
    (-> moon
        (update :x #(+ % (:vx moon)))
        (update :y #(+ % (:vy moon)))
        (update :z #(+ % (:vz moon))))))

(defn get-potential-energy [{:keys [x y z]}]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn get-kinetic-energy [{:keys [vx vy vz]}]
  (+ (Math/abs vx) (Math/abs vy) (Math/abs vz)))

(def final-state
  (loop [moons initial-moons
         t 0]
    (if (= t 1000)
      moons
      (recur (-> moons
                 (update-velocities)
                 (update-positions))
             (inc t)))))

(defn solve []
  (->> final-state
       (map (fn [moon]
              (* (get-kinetic-energy moon) (get-potential-energy moon))))
       (apply +)))

(def init-x-state
  (map (fn [state] [(:x state) (:vx state)]) initial-moons ))

(def init-y-state
  (map (fn [state] [(:y state) (:vy state)]) initial-moons ))

(def init-z-state
  (map (fn [state] [(:z state) (:vz state)]) initial-moons ))

(defn get-x-interval []
  (loop [moons initial-moons
         t 0]
    (let [next-state (-> moons
                         (update-velocities)
                         (update-positions))]
      (if (= (map (fn [moon] [(:x moon) (:vx moon)]) next-state)
             init-x-state)
        (inc t)
        (do
          (recur next-state
                 (inc t)))))))

(defn get-y-interval []
  (loop [moons initial-moons
         t 0]
    (let [next-state (-> moons
                         (update-velocities)
                         (update-positions))]
      (if (= (map (fn [moon] [(:y moon) (:vy moon)]) next-state)
             init-y-state)
        (inc t)
        (do
          (recur next-state
                 (inc t)))))))

(defn get-z-interval []
  (loop [moons initial-moons
         t 0]
    (let [next-state (-> moons
                         (update-velocities)
                         (update-positions))]
      (if (= (map (fn [moon] [(:z moon) (:vz moon)]) next-state)
             init-z-state)
        (inc t)
        (do
          (recur next-state
                 (inc t)))))))
(defn solve2 []
  (let [[x-int y-int z-int] [(get-x-interval) (get-y-interval) (get-z-interval)]]
    (math/lcm x-int (math/lcm y-int z-int))))
;; 318382803780324
