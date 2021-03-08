(ns ^:figwheel-no-load aoc.day11
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

(def left {:n :w
           :w :s
           :s :e
           :e :n})

(def actions
  {"N" (fn [ship val]
         (update ship :n + val))

   "S" (fn [ship val]
         (update ship :s + val))

   "E" (fn [ship val]
         (update ship :e + val))

   "W" (fn [ship val]
         (update ship :w + val))

   "F" (fn [{:keys [dir] :as ship} val]
         (update ship dir + val))

   "R" (fn [{:keys [dir] :as ship} val]
         (let [new-dir (case val
                         90 (right dir)
                         180 (right (right dir))
                         270 (right (right (right dir))))]
           (conj ship [:dir new-dir])))

   "L" (fn [{:keys [dir] :as ship} val]
         (let [new-dir (case val
                         90 (left dir)
                         180 (left (left dir))
                         270 (left (left (left dir))))]
           (conj ship [:dir new-dir])))})

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
                    :dir :e})]
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
