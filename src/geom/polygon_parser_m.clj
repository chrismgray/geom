(ns geom.polygon-parser-m
  (:use [clojure.algo.monads]))

(defmonad polygon-parser-m
  [m-result (fn [poly]
              (fn [pt stack]
                [pt (vec poly) (vec stack)]))
   m-bind (fn [mv f]
            (fn [pt stack]
              (let [result (mv pt stack)]
                (when (not= nil result)
                  (let [[pt new-poly new-stack] result]
                    ((f new-poly) pt new-stack))))))
   m-zero (fn [pt stack]
            nil)
   m-plus (fn [& parsers]
            (fn [pt stack]
              (some identity (map #(% pt stack) parsers))))])
