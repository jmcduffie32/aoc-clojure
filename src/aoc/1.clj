(ns aoc.1
  (:require [clojure.edn :as edn]))

(defn calculate-fuel [mass]
  (let [required-fuel (- (quot mass 3) 2)]
    (cond
      (<= required-fuel 0) 0
      (> required-fuel) (+ required-fuel (calculate-fuel required-fuel)))))

(defn get-fuel-req [mass-string]
  (let [mass (edn/read-string mass-string)]
    (calculate-fuel mass)))

(defn get-values [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map get-fuel-req (line-seq rdr)))))

;; (apply + (get-values "./src/aoc/1_input.txt"))
;; 5106777
