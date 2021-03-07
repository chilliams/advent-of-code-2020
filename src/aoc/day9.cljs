(ns ^:figwheel-no-load aoc.day9
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [cljs.core.logic :as m]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]))

(def example
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse [input]
  (->> input
       string/split-lines
       (map #(js/parseInt % 10))))

(defn pairsums [s i]
  (let [five (take i s)
        ps (for [x five
                 y five
                 :when (not= x y)]
             (+ x y))]
    (set ps)))

(defn solve [input i]
  (let [s (parse input)]
    (loop [s s]
      (let [x (nth s i)]
        (if (contains? (pairsums s i) x)
          (recur (rest s))
          x)))))

(solve example 5)

(go
  (-> "input9"
      xhr/get
      <p!
      (solve 25)
      pprint))

(def invalid-number 675280050)

(defn solve2 [s]
  (loop [i 0
         j 1
         sum (first s)]
    (cond
      (< sum invalid-number) (recur i (inc j) (+ sum (nth s j)))
      (> sum invalid-number) (recur (inc i) j (- sum (nth s i)))
      :else [i j])))

(go
  (let [s (-> "input9"
              xhr/get
              <p!
              parse)
        [i j] (solve2 s)
        ss (for [x (range i j)]
             (nth s x))]
    (pprint (reduce + ss))
    (pprint (+ (reduce min ss) (reduce max ss)))))
