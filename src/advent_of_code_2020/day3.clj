(ns advent-of-code-2020.day3
  (:require [clojure.string :as string]))

(defn- make-move
  [{:keys [map-data width]} pos move]
  (let [new-pos [(+ (first pos) (first move))
                 (mod (+ (second pos) (second move))
                      width)]]
    [new-pos (= "#" (get-in map-data new-pos))]))

(defn- go
  [input move]
  (let [map-data (vec
                   (for [l input]
                     (vec (string/split l #""))))
        toboggan-map {:map-data map-data
                      :depth (count map-data)
                      :width (count (first map-data))}]
    (loop [pos [0 0]
           tree-count 0]
      (cond
        (or (> (first pos) (:depth toboggan-map))
            (> (second pos) (:width toboggan-map)))
        tree-count

        :else
        (let [[new-pos tree?] (make-move toboggan-map
                                         pos
                                         move)]
          (recur new-pos (if tree? (inc tree-count) tree-count)))))))

(defn part1
  [input]
  (go input [1 3]))

(defn part2
  [input]
  (let [moves [[1 1]
               [1 3]
               [1 5]
               [1 7]
               [2 1]]]
    (->> moves
         (map (partial go input))
         (apply *))))
