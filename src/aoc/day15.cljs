(ns ^:figwheel-no-load aoc.day15
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

(defn next-number [x turns]
  (if-let [t (object/get turns x)]
    (let [l (.-length t)
          f (aget t (dec l))
          s (aget t (dec (dec l)))]
      (if (nil? s)
        0
        (- f s)))))

(defn counts [s]
  (->> s
       (map-indexed (fn [idx itm] {itm (list (inc idx))}))
       (apply merge-with into)
       clj->js))

(counts [15 5 1 4 7 0])

(object/get #js{"0" #js[1 2]} "0")

(defn solve [s target]
  (let [counts (counts s)]
    (loop [turn (count s)
           x (last s)]
      (let [n (next-number x counts)]
        (if (>= turn target)
          x
          (do
            (if-let [a (object/get counts n)]
              (.push a (inc turn))
              (object/set counts n #js[(inc turn)]))
            (recur (inc turn) n)))))))

(comment

  (time (pprint (solve [0 3 6] 30000000)))

  (time (pprint (solve [15,5,1,4,7,0] 30000000)))

  )
