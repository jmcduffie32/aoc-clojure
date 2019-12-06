(ns aoc.5
  (:require [clojure.edn :as edn]))


;; (def prog [1 0 0 0 99])
;; (def prog1 [1,1,1,4,99,5,6,0,99])
;; (def puzzle [1,82,26,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0])

(def prog [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,71,224,1001,224,-111,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1102,66,6,225,1102,22,54,225,1,65,35,224,1001,224,-86,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,20,80,225,101,92,148,224,101,-162,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1102,63,60,225,1101,32,48,225,2,173,95,224,1001,224,-448,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,1001,91,16,224,101,-79,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1101,13,29,225,1101,71,70,225,1002,39,56,224,1001,224,-1232,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,14,59,225,102,38,143,224,1001,224,-494,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,30,28,224,1001,224,-840,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,359,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,434,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,449,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,599,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226])




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

(defn run-prog [program, index]
  (let [{:keys [opcode arg1-mode arg2-mode]} (parse-opcode (nth program index))]
    (cond
      (= opcode 1) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result-index (nth program (+ index 3))
                         sum (+ val1 val2)
                         new-program (assoc program result-index sum)]
                     (println "ADDING: " val1 " + " val2 " = " sum)
                     (recur new-program (+ index 4)))
      (= opcode 2) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result-index (nth program (+ index 3))
                         product (* val1 val2)
                         new-program (assoc program result-index product)]
                     (println "MULTIPLYING: " val1 " * " val2 " = " product)
                     (recur new-program (+ index 4)))
      (= opcode 3) (let [input (edn/read-string
                                (do (print "INPUT: ") (flush) (read-line)))
                         result-index (nth program (+ index 1))
                         new-program (assoc program result-index input)]
                     (recur new-program (+ index 2)))
      (= opcode 4) (let [output (nth program (nth program (+ index 1)))]
                     (println "DIAGNOSTIC CODE: " output)
                     (recur program (+ index 2)))
      (= opcode 5) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         new-index (if (= 0 val1) (+ index 3) val2)]
                     (println "JUMP-IF-TRUE: " val1 " ,POINTER => " new-index)
                     (recur program new-index))
      (= opcode 6) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         new-index (if (= 0 val1) val2 (+ index 3))]
                     (recur program new-index))
      (= opcode 7) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result (if (< val1 val2) 1 0)
                         result-index (nth program (+ index 3))
                         new-program (assoc program result-index result)]
                     (recur new-program (+ index 4)))
      (= opcode 8) (let [val1 (get-val program (+ index 1) arg1-mode)
                         val2 (get-val program (+ index 2) arg2-mode)
                         result (if (= val1 val2) 1 0)
                         result-index (nth program (+ index 3))
                         new-program (assoc program result-index result)]
                     (recur new-program (+ index 4)))
      (= opcode 99) program
      :else (do (println "INVALID OP CODE: " opcode) program))))


(defn solve []
  (run-prog prog 0))
