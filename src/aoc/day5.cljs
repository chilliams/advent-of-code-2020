(ns ^:figwheel-no-load aoc.day5
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]))


(def example "FBFBBFFRLR")

(- 127 (/ 128 2))

(defn row [seat]
  (loop [i 0
         [minr maxr] [0 127]
         s 128]
    (case (get seat i)
      "F" (recur (inc i) [minr (- maxr (/ s 2))] (/ s 2))
      "B" (recur (inc i) [(+ minr (/ s 2)) maxr] (/ s 2))
      minr)))

(defn column [seat]
  (loop [i 7
         [minc maxc] [0 7]
         s 8]
    (case (get seat i)
      "L" (recur (inc i) [minc (- maxc (/ s 2))] (/ s 2))
      "R" (recur (inc i) [(+ minc (/ s 2)) maxc] (/ s 2))
      maxc)))

(+ (* (row example) 8)
   (column example))

(defn seat-id [seat]
  (-> (row seat)
      (* 8)
      (+ (column seat))))

(seat-id example)
(seat-id "BFFFBBFRRR")
(seat-id "FFFBBBFRRR")
(seat-id "BBFFBBFRLL")


(go
  (let [seats (->> "input5"
                   xhr/get
                   <p!
                   string/split-lines
                   (map seat-id)
                   (apply sorted-set))]
    (doseq [i (range (last seats))
            :when (not (contains? seats i))]
      (print i))))
