(ns advent.day1
  (:require [clojure.math.combinatorics :refer [combinations]]
            [advent.data :as data]))

(defn get-mistake
  "Determine if `nums` has a complement in the `report`, get the 'mistake' if so."
  [report target & nums]
  (let [sum-of-nums (reduce + nums)
        complement (- target sum-of-nums)]
    (when (contains? report complement)
      (reduce * complement nums))))

(defn find-mistake
  "Find the 'mistake' product from `report`."
  [report year]
  (some (partial get-mistake report year)
        report))

(defn find-combined-mistake
  "Find the 'mistake' product for `n`+1 elements in `report`."
  [report year n]
  (let [combos (combinations report n)]
    (some (partial apply get-mistake report year)
          combos)))

(comment
  (find-mistake data/report 2020)
  (find-combined-mistake data/report 2020 2))
