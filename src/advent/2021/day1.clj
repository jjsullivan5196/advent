(ns advent.2021.day1
  (:require [advent.util :as u]
            [clojure.math.combinatorics :refer [combinations]]))

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

(def example-report
  #{1721
    979
    366
    299
    675
    1456})

(comment
  (find-mistake example-report 2020)

  #_=> 514579

  (find-combined-mistake example-report 2020 2)

  #_=> 241861950

  (let [my-report (->> (u/fname->lines "data/2021/day1.txt")
                       (map u/->int)
                       set)]
    {:first-mistake (find-mistake my-report 2020)
     :n-mistake     (find-combined-mistake my-report 2020 2)})

  #_=> {:first-mistake 842016, :n-mistake 9199664}

  #_...)
