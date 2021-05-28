(ns advent.passport
  (:require [advent.util :as u :refer [->int]]
            [clojure.set :as st]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [clojure.test.check.generators :as gen]))

;; Spec for passport

(do
  (def digits            (set "0123456789"))
  (def hex-digits        (st/union (set "abcdefABCDEF")
                                   digits))

  (spec/def ::birth-year (spec/int-in 1920 (inc 2002)))
  (spec/def ::issued     (spec/int-in 2010 (inc 2020)))
  (spec/def ::expires    (spec/int-in 2020 (inc 2030)))
  (spec/def ::height     (spec/int-in 149  (inc 193)))

  (spec/def ::hair-color (spec/coll-of hex-digits :count 6))
  (spec/def ::id         (spec/coll-of digits :count 9))

  (spec/def ::eye-color  #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

  (spec/def ::passport (spec/keys
                        :req [::eye-color
                              ::id
                              ::expires
                              ::hair-color
                              ::birth-year
                              ::issued
                              ::height])))

(comment
  (let [seed           893918293892189899
        passport-count 3
        passport-gen   (spec/gen ::passport)]
    (->> (range seed (+ seed passport-count))
         (mapv #(gen/generate passport-gen 500 %))))

  #_=> [{:advent.passport/eye-color "blu",
         :advent.passport/id [\0 \6 \2 \2 \5 \6 \8 \7 \4],
         :advent.passport/expires 2027,
         :advent.passport/hair-color [\C \e \3 \8 \e \1],
         :advent.passport/birth-year 1988,
         :advent.passport/issued 2018,
         :advent.passport/height 174}

        {:advent.passport/eye-color "gry",
         :advent.passport/id [\7 \7 \1 \8 \3 \0 \1 \6 \1],
         :advent.passport/expires 2028,
         :advent.passport/hair-color [\1 \d \6 \B \a \D],
         :advent.passport/birth-year 1922,
         :advent.passport/issued 2015,
         :advent.passport/height 172}

        {:advent.passport/eye-color "grn",
         :advent.passport/id [\8 \4 \1 \8 \7 \0 \4 \8 \4],
         :advent.passport/expires 2025,
         :advent.passport/hair-color [\0 \a \7 \b \a \2],
         :advent.passport/birth-year 1974,
         :advent.passport/issued 2018,
         :advent.passport/height 186}]

  #_...)

(defn contains-required-fields?
  "Does this `passport` have every field needed?"
  [passport]
  (st/superset? (->> (keys passport) set)
                #{::eye-color
                  ::id
                  ::expires
                  ::hair-color
                  ::birth-year
                  ::issued
                  ::height}))

;; Conversions

(defn unit-str->cm
  "Take a string `s` like '15in' or '32cm' and convert to centimeters as an int."
  [s]
  (let [n-chars (count s)
        n       (-> (subs s 0 (- n-chars 2))
                    ->int)
        units   (subs s (- n-chars 2))]
    (case units
      "cm" n
      "in" (-> n (* 2.54) int)
      nil)))

(def field-conversions
  "Mapping from input passport fields to a spec'd equivalent."
  {"cid" [::country    identity]
   "ecl" [::eye-color  identity]
   "pid" [::id         vec]
   "hcl" [::hair-color #(->> (seq %) rest vec)]
   "hgt" [::height     unit-str->cm]
   "byr" [::birth-year ->int]
   "iyr" [::issued     ->int]
   "eyr" [::expires    ->int]})

(defn pair->field
  "Take raw [key value] pair from passport and turn it into usable data."
  [[k v]]
  (let [[field-kw ->val] (get field-conversions k)]
    [field-kw (->val v)]))

(defn pairs->passport
  "Get a passport for a sequence of input `pairs`."
  [pairs]
  (->> pairs
       (map pair->field)
       (into {})))

;; Serialization stuff

(defn split-pairs
  "Take string `s` of colon separated pairs and create a sequence of tuples from it."
  [s]
  (->> (s/split s #"\s+")
       (map #(s/split % #"[:]" 2))))

(defn split-records
  "Create sequence of strings representing blank-line delimited records."
  [lines]
  (->> lines
       (partition-by s/blank?)
       (map (partial s/join " "))
       (remove s/blank?)))

(def example-input
  ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
   "byr:1937 iyr:2017 cid:147 hgt:183cm"
   ""
   "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
   "hcl:#cfa07d byr:1929"
   ""
   "hcl:#ae17e1 iyr:2013"
   "eyr:2024"
   "ecl:brn pid:760753108 byr:1931"
   "hgt:179cm"
   ""
   "hcl:#cefa07d eyr:2025 pid:166559648"
   "iyr:2011 ecl:brn hgt:59in"])

(comment
  (unit-str->cm "80in")

  #_=> 203

  (unit-str->cm "20cm")

  #_=> 20

  (pair->field ["ecl" "grn"])

  #_=> [:advent.passport/eye-color "grn"]

  (pair->field ["hcl" "#ae17e1"])

  #_=> [:advent.passport/hair-color [\a \e \1 \7 \e \1]]

  (pair->field ["hgt" "72in"])

  #_=> [:advent.passport/height 182]

  (->> example-input
       split-records)

  #_=> ("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
        "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
        "hcl:#cefa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in")

  (->> example-input
       split-records
       (map split-pairs))

  #_=> ((["ecl" "gry"]
         ["pid" "860033327"]
         ["eyr" "2020"]
         ["hcl" "#fffffd"]
         ["byr" "1937"]
         ["iyr" "2017"]
         ["cid" "147"]
         ["hgt" "183cm"])
        (["iyr" "2013"]
         ["ecl" "amb"]
         ["cid" "350"]
         ["eyr" "2023"]
         ["pid" "028048884"]
         ["hcl" "#cfa07d"]
         ["byr" "1929"])
        (["hcl" "#ae17e1"]
         ["iyr" "2013"]
         ["eyr" "2024"]
         ["ecl" "brn"]
         ["pid" "760753108"]
         ["byr" "1931"]
         ["hgt" "179cm"])
        (["hcl" "#cefa07d"]
         ["eyr" "2025"]
         ["pid" "166559648"]
         ["iyr" "2011"]
         ["ecl" "brn"]
         ["hgt" "59in"]))

  (->> example-input
       split-records
       (map split-pairs)
       (map pairs->passport)
       (mapv (fn [passport]
               {:passport         passport
                :required-fields? (contains-required-fields? passport)
                :valid-passport?  (spec/valid? ::passport passport)})))

  #_=> [{:passport {:advent.passport/eye-color "gry",
                    :advent.passport/id [\8 \6 \0 \0 \3 \3 \3 \2 \7],
                    :advent.passport/expires 2020,
                    :advent.passport/hair-color [\f \f \f \f \f \d],
                    :advent.passport/birth-year 1937,
                    :advent.passport/issued 2017,
                    :advent.passport/country "147",
                    :advent.passport/height 183},
         :required-fields? true,
         :valid-passport? true}
        {:passport {:advent.passport/issued 2013,
                    :advent.passport/eye-color "amb",
                    :advent.passport/country "350",
                    :advent.passport/expires 2023,
                    :advent.passport/id [\0 \2 \8 \0 \4 \8 \8 \8 \4],
                    :advent.passport/hair-color [\c \f \a \0 \7 \d],
                    :advent.passport/birth-year 1929},
         :required-fields? false,
         :valid-passport? false}
        {:passport {:advent.passport/hair-color [\a \e \1 \7 \e \1],
                    :advent.passport/issued 2013,
                    :advent.passport/expires 2024,
                    :advent.passport/eye-color "brn",
                    :advent.passport/id [\7 \6 \0 \7 \5 \3 \1 \0 \8],
                    :advent.passport/birth-year 1931,
                    :advent.passport/height 179},
         :required-fields? true,
         :valid-passport? true}
        {:passport {:advent.passport/hair-color [\c \e \f \a \0 \7 \d],
                    :advent.passport/expires 2025,
                    :advent.passport/id [\1 \6 \6 \5 \5 \9 \6 \4 \8],
                    :advent.passport/issued 2011,
                    :advent.passport/eye-color "brn",
                    :advent.passport/height 149},
         :required-fields? false,
         :valid-passport? false}]

  (let [passports (->> (u/fname->lines "day4-data.txt")
                       split-records
                       (map split-pairs)
                       (map pairs->passport))]
    {:required-fields? (->> passports
                            (filter contains-required-fields?)
                            count)
     :valid-passport?  (->> passports
                            (filter (partial spec/valid? ::passport))
                            count)})

  #_=> {:required-fields? 239, :valid-passport? 188}

  #_...)
