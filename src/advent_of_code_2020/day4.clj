(ns advent-of-code-2020.day4
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn- passport-fields
  [pport]
  (reduce (fn [acc field]
            (let [[field-name field-value] (string/split field #":")]
              (assoc acc (keyword field-name) field-value)))
          {}
          (string/split pport #" ")))

(defn- passport-seq
  [lines]
  (if (empty? lines)
    []
    (let [[pport more] (split-with (partial not= "") lines)]
      (lazy-cat [(passport-fields (string/join \space pport))]
                (passport-seq (rest more))))))

(defn- part1-valid?
  [pport]
  (let [expected-fields #{:byr
                          :iyr
                          :eyr
                          :hgt
                          :hcl
                          :ecl
                          :pid}
        fields (-> pport keys set)
        diff (set/difference fields expected-fields)
        rdiff (set/difference expected-fields fields)]
    (and (empty? rdiff)
         (or (empty? diff)
             (= #{:cid} diff)))))

(defn part1
  [input]
  (let [passports (passport-seq input)]
    (->> passports
         (filter part1-valid?)
         (count))))

(defn- part2-valid?
  [pport]
  (let [eye-colours #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
        parse-int (fn [i]
                    (try
                      (Integer/parseInt i)
                      (catch Exception _
                        0)))]
    (and (part1-valid? pport)
         (<= 1920 (parse-int (:byr pport)) 2002)
         (<= 2010 (parse-int (:iyr pport)) 2020)
         (<= 2020 (parse-int (:eyr pport)) 2030)
         (let [[_ hgt dim] (re-matches #"(\d+)(cm|in)" (:hgt pport))
               hgt (parse-int hgt)]
           (condp = dim
             "cm" (<= 150 hgt 193)
             "in" (<= 59 hgt 76)
             false))
         (re-matches #"#[0-9a-f]{6}" (:hcl pport))
         (contains? eye-colours (:ecl pport))
         (re-matches #"[0-9]{9}" (:pid pport)))))

(defn part2
  [input]
  (let [passports (passport-seq input)]
    (->> passports
         (filter part2-valid?)
         (count))))
