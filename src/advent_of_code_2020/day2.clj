(ns advent-of-code-2020.day2
  (:require [clojure.string :as string]))

(defn- parse-policy
  [policy-defn]
  (let [[_ min-count max-count character] (re-matches #"(\d+)-(\d+) (\w)" policy-defn)]
    {:min (Integer/parseInt min-count)
     :max (Integer/parseInt max-count)
     :char character}))

(defn- policy-matcher
  [policy]
  (let [re (re-pattern (:char policy))]
    (fn [s]
      (<= (:min policy)
          (count (re-seq re s))
          (:max policy)))))

(defn- extract-policy-password
  [line]
  (let [[policy-defn password] (string/split line #": ")]
    {:password password
     :policy-fn (-> policy-defn parse-policy policy-matcher)}))

(defn- go
  [input extract-fn]
  (count
    (for [line input
          :let [{:keys [password policy-fn]} (extract-fn line)]
          :when (policy-fn password)]
      line)))

(defn part1
  [input]
  (go input extract-policy-password))

(defn- new-policy-matcher
  [policy]
  (fn [s]
    (cond
      (< (:min policy) 0)
      false

      (> (:max policy) (count s))
      false

      :else
      (= 1 (count
             (filter #(= (:char policy) (str %))
                     [(.charAt s (dec (:min policy)))
                      (.charAt s (dec (:max policy)))]))))))

(defn- extract-new-policy-password
  [line]
  (let [[policy-defn password] (string/split line #": ")]
    {:password password
     :policy-fn (-> policy-defn parse-policy new-policy-matcher)}))

(defn part2
  [input]
  (go input extract-new-policy-password))
