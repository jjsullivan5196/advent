(ns advent.2021.day5
  (:require [advent.util :as u]))

(defn binary-upper
  "Take the upper half of the range."
  [[begin end]]
  (let [n (-> (- end begin) (/ 2))]
    [(-> begin (+ n) Math/ceil int)
     end]))

(defn binary-lower
  "Take the lower half of the range."
  [[begin end]]
  (let [n (-> (- end begin) (/ 2))]
    [begin
     (-> end (- n) int)]))

(def binary-partition
  "Map the partitions of a seat id to the operator functions."
  {\F binary-lower
   \B binary-upper
   \L binary-lower
   \R binary-upper})

(def front-back
  "For splitting out the front-back partitions from passes."
  #{\F \B})

(defn partitions->n
  "Reduce some partitions over a range to a single number."
  [begin end parts]
  (->> parts
       (map binary-partition)
       (reduce #(%2 %1) [begin end])
       first))

(defn pass->seat
  "Get [row aisle] for a pass `s`."
  [s]
  (let [[rows aisles] (->> (seq s)
                           (partition-by #(contains? front-back %)))
        row           (partitions->n 0 127 rows)
        aisle         (partitions->n 0 7 aisles)]
    [row aisle]))

(def example "FBFBBFFRLR")

(comment
  (->> (seq example)
       (partition-by #(contains? front-back %)))

  #_=> '((\F \B \F \B \B \F \F) (\R \L \R))

  (partitions->n 0 127 [\F \B \F \B \B \F \F])

  #_=> 44

  (pass->seat example)

  #_=> [44 5]

  (->> (u/fname->lines "data/2021/day5.txt")
       (map pass->seat)
       (map (fn [[row aisle]]
              (-> row (* 8) (+ aisle))))
       (reduce max))

  #_=> 926

  (->> (u/fname->lines "data/2021/day5.txt")
       (map pass->seat)
       (map (fn [[row aisle]]
              (-> row (* 8) (+ aisle))))
       sort
       (partition 2)
       (filter (fn [[x y]]
                 (-> (- x y)
                     Math/abs
                     (= 2)))))

  #_=> '((656 658))

  #_...)
