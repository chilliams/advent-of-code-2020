(ns ^:figwheel-no-load aoc.day16
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]
   [goog.object :as object])
  (:import
   [goog.string StringBuffer]))

(def example
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
")

(defn validator [[[a b] [c d]]]
  (fn [value]
    (cond
      (<= a value b) true
      (<= c value d) true
      :else false)))

(defn parse-fields [input]
  (->>
   (for [[_ field a b c d]
         (re-seq #"(.*): (\d*)-(\d*) or (\d*)-(\d*)" input)]
     (let [[a b c d] (map #(js/parseInt % 10) [a b c d])]
       {field (validator [[a b] [c d]])}))
   (into {})))

(defn parse-ticket [line]
  (->> (string/split line ",")
       (map #(js/parseInt % 10))))

(defn parse-tickets [input]
  (->> input
       string/split-lines
       (filter #(.includes % ","))
       (map parse-ticket)))

(defn value-validator [fields]
  (let [validators (vals fields)]
    (fn [v]
      (->> validators
           (map #(% v))
           (some true?)))))

(defn ticket-validator [vv]
  (fn [t]
    (->> t
         (map (fn [v]
                (if (vv v)
                  nil
                  v)))
         (filter some?))))

(defn solve [input]
  (let [fields (parse-fields input)
        vv (value-validator fields)
        tv (ticket-validator vv)
        tickets (parse-tickets input)]
    (->> tickets
         (map tv)
         flatten
         (reduce +))))

(go
  (-> "input16"
      xhr/get
      <p!
      solve
      pprint))
