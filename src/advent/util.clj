(ns advent.util
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn fname->lines
  "Open resource `fname` and get its contents line by line."
  [fname]
  (-> fname
      io/resource
      io/reader
      line-seq))

(defn ->int
  "Parse non-empty string `s` as an int."
  [s]
  (-> (and (not (s/blank? s))
           (Integer/parseInt s))
      (or nil)))
