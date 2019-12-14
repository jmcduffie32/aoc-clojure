(ns aoc.13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [incanter.core :as core]
            [incanter.charts :as charts]
            [incanter.datasets]))

(def input-file "./resources/13_1.csv")
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
    (cond
      (= mode 0) (get program value-at-index)
      (= mode 1) (get program index)
      (= mode 2) (get program (+ value-at-index rel-base)))))

(defn get-output-index [program index mode rel-base]
  (let [value-at-index (get program index)]
    (cond
      (= mode 0) value-at-index
      (= mode 2) (+ value-at-index rel-base))))

(defn get-joystick-input [{:keys [ball paddle] :as state}]
  (cond
    (> (:x ball) (:x paddle)) 1
    (< (:x ball) (:x paddle)) -1
    (= (:x ball) (:x paddle)) 0))

(def init-input {:input 1
                 :index 0
                 :output nil
                 :output-buffer []
                 :score 0
                 :ball {:x 0 :y 0}
                 :paddle {:x 0 :y 0}
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
      (= opcode 3) (let [input (get-joystick-input state)
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


(defn solve-puzzle []
  (loop [prog-state init-input
         output-list []]
    (let [new-prog-state (run-prog prog-state)
          output (:output new-prog-state)
          output-type (:output-type new-prog-state)]
      (if (:has-halted new-prog-state)
        output-list
        (recur new-prog-state
               (conj output-list output))))))

;; (def block-count (->> (solve-puzzle)
;;                      (partition 3)
;;                      (map #(nth % 2))
;;                      (filter #(= 2 %))
;;                      (count)))
(defn handle-output [{:keys [output-buffer] :as state}]
  (if (= (count output-buffer) 3)
    (let [[x y id] output-buffer]
      (cond
        (and (= x -1) (= y 0)) (-> state
                                   (assoc :output-buffer [])
                                   (assoc :score id))
        (= id 3) (-> state
                     (assoc :paddle {:x x :y y})
                     (assoc :output-buffer []))
        (= id 4) (-> state
                     (assoc :ball {:x x :y y})
                     (assoc :output-buffer []))
        :else (assoc state :output-buffer [])))
    state))

(defn solve2 []
  (loop [prog-state init-input]
    (println (:index prog-state))
    ;; (println prog-state)
    (let [{:keys [output-buffer output] :as new-prog-state} (run-prog prog-state)]
      (if (:has-halted new-prog-state)
        (:score new-prog-state)
        (-> new-prog-state
            (assoc :output-buffer (conj output-buffer output))
            (handle-output)
            (recur))))))

