(ns advent.day3
  (:require [clojure.java.io :as io]))

(defn char->gridspace
  "Convert character `c` from input into its corresponding space in the grid."
  [c]
  (condp = c
    \. :open
    \# :blocked))

(defn load-grid
  "Get grid data from file `fname`."
  [fname]
  (let [lines       (-> fname io/resource io/reader line-seq)
        grid-height (count lines)
        grid-width  (-> lines first count)
        grid-spaces (->> lines
                         (mapcat seq)
                         (mapv char->gridspace))]
    {:width  grid-width
     :height grid-height
     :spaces grid-spaces}))

(defn coordinate->grid-index
  [{:as grid :keys [width]} [x y]]
  (-> y (* width) (+ x)))

#_(defn slope->path
    [{:as grid :keys [width height]} [run fall]]
    (let [total-steps (-> height (/ fall) int)]
      (->> (range total-steps)
           (map (fn [step] [(-> run (* step) (mod width))
                            (* fall step)])))))
(defn slope->path
  [{:as grid :keys [width height]} [run fall]]
  (let [ys (range 0 height fall)
        xs (->> (range width)
                (map (fn [n] (-> run (* n) (mod width))))
                (cycle))]
    (map vector xs ys)))

(defn path->spaces
  [{:as grid :keys [spaces]} path-coords]
  (->> path-coords
       (map (fn [xy]
              (let [idx (coordinate->grid-index grid xy)]
                (nth spaces idx))))))

(comment
  (load-grid "day3-data.txt")

  #_=> {:width 31,
        :height 323,
        :spaces [:open :blocked :open :open :open #_...]}

  (let [grid   (load-grid "day3-data.txt")
        path   (slope->path grid [3 1])
        spaces (path->spaces grid path)
        hits   (->> spaces frequencies :blocked)]
    {:path   (take 5 path)
     :spaces (take 5 spaces)
     :hits   hits})

  #_=> {:path   '([0 0] [3 1] [6 2] [9 3] [12 4] #_...),
        :spaces '(:open :open :open :open :blocked #_...),
        :hits   173}

  (let [grid       (load-grid "day3-data.txt")
        slopes     [[1 1] [3 1] [5 1] [7 1] [1 2]]
        all-hits   (->> slopes
                        (map (fn [run-fall]
                               (->> (slope->path grid run-fall)
                                    (path->spaces grid)
                                    frequencies
                                    :blocked))))]
    (reduce * all-hits))

  #_=> 4385176320

  #_...)
