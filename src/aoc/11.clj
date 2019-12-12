(ns aoc.11
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [incanter.core :as core]
            [incanter.charts :as charts]
            [incanter.datasets]))

(def input-file "./resources/11_1.csv")
;; (def input-file "./resources/10_test.csv")

(def prog
  (-> (slurp input-file)
      (str/trim-newline)
      (str/split #",")
      (->> (mapv edn/read-string))))


(defn parse-opcode [opcode]
  {:opcode (if (= opcode 99)
             99
             (mod opcode 10))
   :arg1-mode (-> opcode
                  (mod 1000)
                  (quot 100))
   :arg2-mode (-> opcode
                  (mod 10000)
                  (quot 1000))
   :arg3-mode (-> opcode
                  (mod 100000)
                  (quot 10000))
   })

(defn get-val [program index mode rel-base]
  (let [value-at-index (get program index)]
    ;; (println "mode:" mode
    ;;          "index:" index
    ;;          "rel-base:" rel-base
    ;;          "value-at-index:" value-at-index)
    (cond
      (= mode 0) (get program value-at-index)
      (= mode 1) (get program index)
      (= mode 2) (get program (+ value-at-index rel-base)))))

(defn get-output-index [program index mode rel-base]
  (let [value-at-index (get program index)]
    ;; (println "mode:" mode
    ;;          "index:" index
    ;;          "rel-base:" rel-base
    ;;          "value-at-index:" value-at-index)
    (cond
      (= mode 0) value-at-index
      (= mode 2) (+ value-at-index rel-base))))

(defn create-position-key [position]
  (str (:x position) ":" (:y position)))

(defn get-current-color [board-state]
  (let [position (:position board-state)
        key (create-position-key position)
        current-color (get (:visited board-state) key :black)]
    (if (= current-color :black) 0 1)))

(def init-input {:input 1
                 :index 0
                 :output nil
                 :output-type :color
                 :input-type :input
                 :program (apply conj prog (take 10000 (repeat 0)))
                 :rel-base 0
                 :board {:position {:x 0 :y 0}
                         :direction :up
                         :visited {"0:0" :white}}})

(defn run-prog [state]
  (let [{:keys [program index input-type rel-base]} state
        {:keys [opcode arg1-mode arg2-mode arg3-mode]}
        (parse-opcode (nth program index))]
    ;; (println "opcode:" opcode
    ;;          "arg1-mode:" arg1-mode
    ;;          "arg2-mode:" arg2-mode
    ;;          "arg3-mode:" arg3-mode
    ;;          "index:" index
    ;;          "rel-base:" rel-base)
    (cond
      (= opcode 1) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result-index (get-output-index program (+ index 3) arg3-mode rel-base)]
                     (-> state
                         (assoc :program (assoc program result-index (+ val1 val2)))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 2) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result-index (get-output-index program (+ index 3) arg3-mode rel-base)
                         product (* val1 val2)]
                     (-> state
                         (assoc :program (assoc program result-index product))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 3) (let [input (get-current-color (:board state))
                         result-index (get-output-index program (+ index 1) arg1-mode rel-base)]
                     (-> state
                         (assoc :program (assoc program result-index input))
                         (assoc :index (+ index 2))
                         (assoc :input-type :input)
                         (recur)))
      (= opcode 4) (let [output (get-val program (+ index 1) arg1-mode rel-base)]
                     (-> state
                         (assoc :index (+ index 2))
                         (assoc :output output)
                         ;; (recur)
                         ))
      (= opcode 5) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)]
                     (-> state
                         (assoc :index (if (= 0 val1) (+ index 3) val2))
                         (recur)))
      (= opcode 6) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)]
                     (-> state
                         (assoc :index (if (= 0 val1) val2 (+ index 3)))
                         (recur)))
      (= opcode 7) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result-index (get-output-index program (+ index 3) arg3-mode rel-base)
                         result (if (< val1 val2) 1 0)]
                     (-> state
                         (assoc :program (assoc program result-index result))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 8) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result (if (= val1 val2) 1 0)
                         result-index (get-output-index program (+ index 3) arg3-mode rel-base)]
                     (-> state
                         (assoc :program (assoc program result-index result))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 9) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)]
                     ;; (println val1 rel-base (+ val1 rel-base))
                     (-> state
                         (assoc :rel-base (+ val1 rel-base))
                         (assoc :index (+ index 2))
                         (recur)))
      (= opcode 99) (-> state
                        (assoc :has-halted true))
      :else (do (println "INVALID OP CODE: " opcode) state))))



(defn get-new-direction [current-direction turn]
  (if (= turn 0)
    (cond
      (= current-direction :up) :left
      (= current-direction :down) :right
      (= current-direction :left) :down
      (= current-direction :right) :up)
    (cond
      (= current-direction :up) :right
      (= current-direction :down) :left
      (= current-direction :left) :up
      (= current-direction :right) :down)))

(defn move-in-direction [position direction]
  (cond
    (= direction :up) (update position :y inc)
    (= direction :down) (update position :y dec)
    (= direction :left) (update position :x dec)
    (= direction :right) (update position :x inc)))

(defn update-position [board-state turn]
  (let [current-direction (:direction board-state)
        new-direction (get-new-direction current-direction turn)
        new-position (move-in-direction (:position board-state) new-direction)]
    (-> board-state
        (assoc :position new-position)
        (assoc :direction new-direction))))


(defn update-color [board-state output]
  (let [color (if (= output 0) :black :white)
        position (:position board-state)
        visited (:visited board-state)]
    (assoc-in board-state
              [:visited (create-position-key position)]
              color)))

(defn update-board-state [board-state output output-type]
  (if (= output-type :direction)
    (update-position board-state output)
    (update-color board-state output)))


(defn toggle-output-type [prog-state]
  (if (= (:output-type prog-state) :direction)
    (assoc prog-state :output-type :color)
    (assoc prog-state :output-type :direction)))

(defn solve-puzzle []
  (loop [prog-state init-input]
    (let [new-prog-state (run-prog prog-state)
          output (:output new-prog-state)
          output-type (:output-type new-prog-state)]
      (if (:has-halted new-prog-state)
        new-prog-state
        (recur (-> new-prog-state
                   (toggle-output-type)
                   (assoc :board
                          (update-board-state
                           (:board new-prog-state) output output-type))))))))

(def all-points
  (-> (solve-puzzle)
      (:board)
      (:visited)
      (->>
       ;; (filter (fn [[k v]] (= v :white)))
           (map #(str/split (first %) #":"))
           (map (fn [v] {:x (edn/read-string (first v)) :y (edn/read-string (second v))})))))

(def visited
  (-> (solve-puzzle)
      (:board)
      (:visited)))

(def points
  (-> (solve-puzzle)
      (:board)
      (:visited)
      (->> (filter (fn [[k v]] (= v :white)))
           (map #(str/split (first %) #":"))
           (map (fn [v] {:x (edn/read-string (first v)) :y (edn/read-string (second v))})))))

(def xs (map :x points))
(def ys (map :y points))
