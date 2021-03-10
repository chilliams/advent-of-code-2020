(ns ^:figwheel-no-load aoc.day13
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr])
  (:import
   [goog.string StringBuffer]))

(def example
  "939
7,13,x,x,59,x,31,19
")

(defn find-bus [t busses]
  (->> busses
       (filter #(= 0 (mod t %)))
       (first)))

(defn find-time [t busses]
  (loop [t t]
    (if-let [bus (find-bus t busses)]
      [t bus]
      (recur (inc t)))))

(defn parse [input]
  (let [[t bs] (string/split-lines input)]
    [(js/parseInt t 10)
     (->> (string/split bs ",")
          (filter (partial not= "x"))
          (map #(js/parseInt % 10)))]))

(defn solve [input]
  (let [[t busses] (parse input)
        [bt bus] (find-time t busses)]
    (* (- bt t)
       bus)))

(solve example)

(go
  (-> "input13"
      xhr/get
      <p!
      solve
      pprint))
