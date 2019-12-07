(ns aoc.7
  (:require [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))


(def input-file "./resources/7_1.csv")
;; (def input-file "./resources/7_1_test.csv")
;; (def input-file "./resources/7_2_test.csv")

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
                  (quot 100)
                  (* 100))
   :arg2-mode (-> opcode
                  (mod 10000)
                  (quot 1000)
                  (* 1000))})

(defn get-val [program index mode]
  (if (= mode 0)
    (nth program (nth program index))
    (nth program index)))

(def amps [{:input 0
            :index 0
            :output nil
            :input-type :phase
            :program prog}
           {:input 0
            :index 0
            :output nil
            :input-type :phase
            :program prog}
           {:input 0
            :index 0
            :output nil
            :input-type :phase
            :program prog}
           {:input 0
            :index 0
            :output nil
            :input-type :phase
            :program prog}
           {:input 0
            :index 0
            :output nil
            :input-type :phase
            :program prog}
           ])

(defn run-prog [state]
  (let [{:keys [program index input-type]} state
        {:keys [opcode arg1-mode arg2-mode]} (parse-opcode (nth program index))]
    (cond
      (= opcode 1) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result-index (nth program (+ index 3))]
                     (println "val1" val1 "val2" val2)
                     (-> state
                         (assoc :program (assoc program result-index (+ val1 val2)))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 2) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result-index (nth program (+ index 3))
                         product (* val1 val2)]
                     (-> state
                         (assoc :program (assoc program result-index product))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 3) (let [input ((:input-type state) state)
                         result-index (nth program (+ index 1))]
                     (-> state
                         (assoc :program (assoc program result-index input))
                         (assoc :index (+ index 2))
                         (assoc :input-type :input)
                         (recur)))
      (= opcode 4) (let [output (nth program (nth program (+ index 1)))]
                     (-> state
                         (assoc :index (+ index 2))
                         (assoc :output output)))
      (= opcode 5) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)]
                     (-> state
                         (assoc :index (if (= 0 val1) (+ index 3) val2))
                         (recur)))
      (= opcode 6) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)]
                     (-> state
                         (assoc :index (if (= 0 val1) val2 (+ index 3)))
                         (recur)))
      (= opcode 7) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result (if (< val1 val2) 1 0)
                         result-index (nth program (+ index 3))]
                     (-> state
                         (assoc :program (assoc program result-index result))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 8) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result (if (= val1 val2) 1 0)
                         result-index (nth program (+ index 3))]
                     (-> state
                         (assoc :program (assoc program result-index result))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 99) state
      :else (do (println "INVALID OP CODE: " opcode) program))))

(defn get-thrust [phase-settings input]
  (let [[phase & rest] phase-settings
        state {:program prog
               :index 0
               :input input
               :output nil
               :phase phase
               :input-type :phase}
        output-state (run-prog state)]
    (if (empty? rest)
      (:output output-state)
      (recur rest (:output output-state)))))

(defn solve []
  (->> (combo/permutations [0 1 2 3 4])
       (map (fn [setting] (get-thrust setting 0)))
       (apply max)))

(defn solve2 []
  (->> (combo/permutations [5 6 7 8 9])
       (map (fn [setting] (get-thrust setting 0)))
       (apply max)))
