(ns ^:figwheel-no-load aoc.day12.part2
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
  "F10
N3
F7
R90
F11")

(defn parse-step [step]
  [(first step)
   (js/parseInt (.slice step 1) 10)])

(def right {:n :e
            :e :s
            :s :w
            :w :n})

(defn rot-right [wp]
  (sets/rename-keys wp right))

(def left {:n :w
           :w :s
           :s :e
           :e :n})

(defn rot-left [wp]
  (sets/rename-keys wp left))

(def actions
  {"N" (fn [ship val]
         (update-in ship [:waypoint :n] + val))

   "S" (fn [ship val]
         (update-in ship [:waypoint :s] + val))

   "E" (fn [ship val]
         (update-in ship [:waypoint :e] + val))

   "W" (fn [ship val]
         (update-in ship [:waypoint :w] + val))

   "F" (fn [{:keys [waypoint] :as ship} val]
         (nth (iterate (partial merge-with + waypoint) ship) val))

   "R" (fn [{:keys [waypoint] :as ship} val]
         (let [new-waypoint
               (case val
                 90 (rot-right waypoint)
                 180 (rot-right (rot-right waypoint))
                 270 (rot-right (rot-right (rot-right waypoint))))]
           (conj ship [:waypoint new-waypoint])))

   "L" (fn [{:keys [waypoint] :as ship} val]
         (let [new-waypoint
               (case val
                 90 (rot-left waypoint)
                 180 (rot-left (rot-left waypoint))
                 270 (rot-left (rot-left (rot-left waypoint))))]
           (conj ship [:waypoint new-waypoint])))})

(defn parse-input [input]
  (->> input
       string/split-lines
       (map parse-step)))

(parse-input example)

(defn manhattan-dist [{:keys [n s e w]}]
  (+ (js/Math.abs (- n s))
     (js/Math.abs (- e w))))

(defn solve [steps]
  (let [ship (atom {:n 0 :s 0 :e 0 :w 0
                    :waypoint {:n 1 :s 0 :e 10 :w 0}})]
    (doseq [[dir val] steps
            :let [action (actions dir)]]
      (swap! ship action val))
    @ship))

(-> example
    parse-input
    solve
    manhattan-dist)

(go
  (-> "input12"
      xhr/get
      <p!
      parse-input
      solve
      manhattan-dist
      print))
