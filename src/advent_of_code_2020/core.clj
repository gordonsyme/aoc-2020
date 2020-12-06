(ns advent-of-code-2020.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]))

(defmacro inspect
  [form]
  `(let [result# ~form]
     (prn (quote ~form) "is" result#)
     result#))

(defn -main
  [input-file day problem]
  (let [ns-sym (symbol (str "advent-of-code-2020." day))]
    (require ns-sym)
    (let [f (ns-resolve ns-sym (symbol problem))]
      (with-open [r (io/reader input-file)]
        (pprint (f (line-seq r)))))))
