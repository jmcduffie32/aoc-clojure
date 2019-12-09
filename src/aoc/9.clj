(ns aoc.9
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; (def input-file "./resources/9_1.csv")
(def input-file "./resources/9_test.csv")

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
                  (* 1000))
  :arg3-mode (-> opcode
                 (mod 100000)
                 (quot 10000)
                 (* 10000))})

(defn get-val [program index mode rel-base]
  (do (println "mode:" mode "index:" index "rel-base:" rel-base)
      (cond
        (= mode 0) (nth program (nth program index))
        (= mode 1) (nth program index)
        (= mode 2) (nth program (+ index rel-base)))))

(def init-input {:input 0
                 :index 0
                 :output nil
                 :input-type :phase
                 :program prog
                 :rel-base 0})

(defn run-prog [state]
  (let [{:keys [program index input-type rel-base]} state
        {:keys [opcode arg1-mode arg2-mode arg3-mode]}
        (parse-opcode (nth program index))]
    (cond
      (= opcode 1) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result-index (get-val program (+ index 3) arg3-mode rel-base)]
                     (-> state
                         (assoc :program (assoc program result-index (+ val1 val2)))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 2) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result-index (get-val program (+ index 3) arg3-mode rel-base)
                         product (* val1 val2)]
                     (-> state
                         (assoc :program (assoc program result-index product))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 3) (let [input ((:input-type state) state)
                         result-index (get-val program (+ index 1) arg1-mode rel-base)]
                     (-> state
                         (assoc :program (assoc program result-index input))
                         (assoc :index (+ index 2))
                         (assoc :input-type :input)
                         (recur)))
      (= opcode 4) (let [output (get-val program (+ index 1) arg1-mode rel-base)]
                     (println output)
                     (-> state
                         (assoc :index (+ index 2))
                         (assoc :output output)))
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
                         result-index (get-val program (+ index 3) arg3-mode rel-base)
                         result (if (< val1 val2) 1 0)]
                     (-> state
                         (assoc :program (assoc program result-index result))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 8) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)
                         val2 (get-val program (+ index 2) arg2-mode rel-base)
                         result (if (= val1 val2) 1 0)
                         result-index (get-val program (+ index 3) arg3-mode rel-base)]
                     (-> state
                         (assoc :program (assoc program result-index result))
                         (assoc :index (+ index 4))
                         (recur)))
      (= opcode 9) (let [val1 (get-val program (+ index 1) arg1-mode rel-base)]
                     (println val1 rel-base (+ val1 rel-base))
                     (-> state
                         (assoc :rel-base (+ val1 rel-base))
                         (assoc :index (+ index 2))
                         (recur)))
      (= opcode 99) (-> state
                        (assoc :has-halted true))
      :else (do (println "INVALID OP CODE: " opcode) program))))

(defn solve []
  (run-prog init-input))
