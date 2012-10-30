(ns parsers.unger
  (:require [clojure.string    :as str]
            [parsers.partition :refer [partition-into-buckets]]))

(def parse)

(defn- find-first [f s]
  (first (filter identity (map f s))))

(defn- buckets [rule tokens]
  (partition-into-buckets (count rule) tokens))

(defn- valid-pair? [grammar [rule-token bucket]]
  (cond
    (keyword? rule-token)
      (parse grammar bucket rule-token)
    (and
      (string? rule-token)
      (= (count bucket) 1)
      (= rule-token (first bucket)))
      (first bucket)
    :otherwise
      nil))

(defn- check-rule [grammar rule buckets]
  (let [sizes-ok (= (count buckets) (count rule))
        valid-pairs (map
                      (partial valid-pair? grammar)
                      (map vector rule buckets))]
    (if (and sizes-ok (every? identity valid-pairs))
      valid-pairs
      nil)))

(defn- check-bucket-sets [grammar tokens rule]
  (let [bucket-sets (buckets rule tokens)]
    (find-first (partial check-rule grammar rule) bucket-sets)))

(defn parse [grammar tokens rule-token]
  (let [choices (grammar rule-token)
        match   (find-first (partial check-bucket-sets grammar tokens) choices)]
    (if match
      [rule-token (vec match )]
      nil)))

;; Examples
(def good-tokens (re-seq #"\S" "((x + (x - x + x)) + x) / (x * x)"))
(def bad-tokens (re-seq #"\S" "x x x"))

(def grammar
  {:operator
     [["+"]
      ["-"]
      ["*"]
      ["/"]]
   :expression
     [["x"]
      ["(" :expression ")"]
      [:expression :operator :expression]]})

(defn- pretty-print
  ([tree] (pretty-print tree 0))
  ([tree indent]
    (cond
      (string? tree)
        (do
          (dotimes [column indent] (print "  "))
          (print \")
          (print tree)
          (println \"))
      (keyword? (first tree))
        (do
          (dotimes [column indent] (print "  "))
          (println (first tree))
          (doseq [child (rest tree)]
            (pretty-print child (inc indent))))
      :otherwise
        (doseq [subtree tree]
          (pretty-print subtree (inc indent))))))

(pretty-print (parse grammar good-tokens :expression))
(pretty-print (parse grammar bad-tokens  :expression))
