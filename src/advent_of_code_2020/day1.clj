(ns advent-of-code-2020.day1)

(defn gen-pairs
  [[x & xs]]
  (if (nil? x)
    []
    (lazy-cat (map (partial vector x) xs)
              (gen-pairs xs))))

(defn day1
  [inputs]
  (->> (gen-pairs inputs)
       (filter (fn [[x y]] (= 2020 (+ x y))))
       first
       (apply *)))

(defn gen-triples
  [[x y & xs]]
  (cond
    (or (nil? x) (nil? y))
    []

    :else
    (lazy-cat (map (partial vector x y) xs)
              (gen-triples (concat [x] xs))
              (gen-triples (concat [y] xs)))))

(defn day1-part2
  [inputs]
  (some->> (gen-triples inputs)
           (filter (fn [[x y z]] (= 2020 (+ x y z))))
           first
           (apply *)))
