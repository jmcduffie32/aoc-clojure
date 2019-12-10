(ns aoc.10 (:require [clojure.string :as str]))

(def input-file "./resources/10_test.txt")

(defn solve []
  (-> input-file
      (slurp)
      (str/split-lines)))



