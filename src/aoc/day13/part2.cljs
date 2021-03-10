(ns ^:figwheel-no-load aoc.day13.part2
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

(defn parse [input]
  (let [[_ bs] (string/split-lines input)]
    (->> (string/split bs ",")
         (map-indexed (fn [i v] [i (js/parseInt v 10)]))
         (filter #(-> % second js/isNaN not)))))

(defn answer? [guess busses]
  (letfn [(fits? [[i b]]
            (not= 0 (mod (+ i guess) b)))]
    (->> busses
         (filter fits?)
         count
         (= 0))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))

(defn lcb [busses]
  (apply lcmv (map second busses)))

(defn solve [input]
  (let [all-busses (parse input)
        [_ bus] (first all-busses)]
    (loop [guess bus
           i bus
           n 1
           busses (take n all-busses)]
      (if (answer? guess busses)
        (if (= busses all-busses)
          guess
          (recur guess
                 (lcb busses)
                 (inc n)
                 (take (inc n) all-busses)))
        (recur (+ i guess) i n busses)))))

;; (print (solve example))

(comment
  (go
    (-> "input13"
        xhr/get
        <p!
        solve
        print))
  )
