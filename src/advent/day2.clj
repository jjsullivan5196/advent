(ns advent.day2
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defmacro delay-seq
  "Turn `forms` into a lazily-evaluated sequence."
  [& forms]
  (let [delayed-forms (mapv (fn [fm] `(delay ~fm)) forms)]
    `(map deref ~delayed-forms)))

(defn xor?
  "Check if `s` contains 1 and only 1 true value"
  [s]
  (->> s
       (filter true?)
       (take 2)
       count
       (= 1)))

(defn valid-repetition?
  "Check if `password` has `needs-char` at least `min-repeat` and at most `max-repeat` times."
  [{:as data :keys [password needs-char params]}]
  (let [[min-repeat max-repeat] params
        char-count              (count (filter #(= needs-char %) password))]
    (and (>= char-count min-repeat)
         (<= char-count max-repeat))))

(defn valid-positions?
  "Check if `password` has `needs-char` in one position at most."
  [{:as data :keys [password needs-char params]}]
  (let [[first-pos second-pos] params]
    (xor? (delay-seq (= (nth password (dec first-pos))
                        needs-char)
                     (= (nth password (dec second-pos))
                        needs-char)))))

(defn deserialize-data
  "Convert `line` from input into useable data."
  [line]
  (let [[raw-params [needs-char] password] (s/split line #" " 3)
        split-params                       (s/split raw-params #"-" 2)
        params                             (mapv #(Integer/parseInt %)
                                                 split-params)
        #_params                             #_(->> (s/split raw-params #"-" 2)
                                                    (map #(Integer/parseInt %)))]
    
    {:password   password
     :needs-char needs-char
     :params     params}))

#_(defn get-test-data
  "Convert all passwords from test data into useable form."
  [file-name]  
  (->> file-name
       io/resource
       io/reader
       line-seq
       (mapv deserialize-data)))

(defn get-test-data
  "Convert all passwords from test data into useable form."
  [file-name]
  (let [lines (-> file-name
                  io/resource
                  io/reader
                  line-seq)
        data (mapv deserialize-data
                   lines)]
    data))

(comment
  ;; structure of advent test data
  (deserialize-data "1-3 b: cdefg")

  #_=> {:password "cdefg", :needs-char \b, :params [1 3]}

  (get-test-data "day2-data.txt")

  #_=> [{:password "kkkkhkkkkkkkkkk", :needs-char \k, :params [1 5]}
        {:password "blkqhtxfgktdkxzkksk", :needs-char \k, :params [5 7]}
        {:password "xxxxxxxxxxxxxxlf", :needs-char \x, :params [15 16]}
        {:password "fjpvj", :needs-char \j, :params [3 5]}
        {:password "zsxjrxkgxxxxxxxmxgxf", :needs-char \x, :params [17 20]}
        {:password "swjzmmmlx", :needs-char \m, :params [5 6]}
        {:password "vqdn", :needs-char \v, :params [2 4]}
        {:password "thllsbqtgdsf", :needs-char \t, :params [8 12]}
        {:password "vpbrjcbhnwqhhphxjk", :needs-char \h, :params [10 17]}
        {:password "zpwpppkqbpkpppp", :needs-char \p, :params [8 9]}
        {:password "wtxxts", :needs-char \t, :params [5 6]}
        {:password "vfsvhgvvhh", :needs-char \v, :params [3 8]}
        {:password "kvvvm", :needs-char \v, :params [1 3]}
        {:password "bwkqpdgwrbwjxrtqlwbw", :needs-char \w, :params [8 16]}
        {:password "sssssssbs", :needs-char \s, :params [3 8]}
        {:password "wwwwwcwwww", :needs-char \w, :params [6 9]}
        {:password "rvwrrlxbrjhp", :needs-char \r, :params [5 10]}
        {:password "rbnlkkrjphnnxpw", :needs-char \r, :params [1 4]}]

  ;; testing
  (let [valid-chars {:password   "abcde"
                     :needs-char \a
                     :params     [1 3]}]

    {:valid-repetition? (valid-repetition? valid-chars)
     :valid-positions?  (valid-positions? valid-chars)})

  #_=> {:valid-repetition? true, :valid-positions? true}
  

  (let [invalid-chars {:password   "cdefg"
                       :needs-char \b
                       :params     [1 3]}]

    {:valid-repetition? (valid-repetition? invalid-chars)
     :valid-positions?  (valid-positions? invalid-chars)})

  #_=> {:valid-repetition? false, :valid-positions? false}

  
  ;; solutions
  (let [all-data          (get-test-data "day2-data.txt")
        correct-repeats   (filter valid-repetition? all-data)
        correct-positions (filter valid-positions? all-data)]

    {:correct-repeats   (count correct-repeats)
     :correct-positions (count correct-positions)})

  #_=> {:correct-repeats 519, :correct-positions 708}


  #_...)
