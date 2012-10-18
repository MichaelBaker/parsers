(ns parsers.unger-test
  (:use clojure.test
        parsers.unger))

(def grammar
  {:expr   [[:expr "+" :term]   [:term]]
   :term   [[:term "*" :factor] [:factor]]
   :factor [["(" :expr ")"]     ["i"]]})

(deftest unger
  (testing "i+i")
  (testing "(i+i)")
  (testing "(i+i)*i"))
