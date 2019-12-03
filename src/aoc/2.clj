(ns aoc.2)

(def prog [1 0 0 0 99])
(def prog1 [1,1,1,4,99,5,6,0,99])
(def puzzle [1,82,26,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0])

(def desired-value 19690720)

(defn run-prog [program, index]
  (let [opcode (nth program index)]
    (cond
      (= opcode 1) (let [val1 (nth program (nth program (+ index 1)))
                         val2 (nth program (nth program (+ index 2)))
                         result-index (nth program (+ index 3))
                         sum (+ val1 val2)
                         new-program (assoc program result-index sum)]
                     (run-prog new-program (+ index 4)))
      (= opcode 2) (let [val1 (nth program (nth program (+ index 1)))
                         val2 (nth program (nth program (+ index 2)))
                         result-index (nth program (+ index 3))
                         product (* val1 val2)
                         new-program (assoc program result-index product)]
                     (run-prog new-program (+ index 4)))
      (= opcode 99) program
      :else program)))

(defn find-noun-verb [program desired-value]
  (doseq [noun (range 0 99)
          verb (range 0 99)]
    (let [test-program (assoc (assoc program 1 noun) 2 verb)]
      (if (= (nth (run-prog test-program 0) 0) desired-value)
        (println noun verb (+ verb (* 100 noun)))))))


