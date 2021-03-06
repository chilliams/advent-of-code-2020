(ns ^:figwheel-no-load aoc.day1
  (:require
   [clojure.string :as string]
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [goog.labs.net.xhr :as xhr]))

;; part 1
(go
  (let [input (->> "input1"
                   xhr/get
                   <p!
                   string/split-lines
                   (map int))
        output (for [x input
                     y input
                     :when (= (+ x y) 2020)]
                 [x y])]
    (->> output
         first
         (apply *)
         print)))

;; part 2
(go
  (let [input (->> "input1"
                   xhr/get
                   <p!
                   string/split-lines
                   (map int))
        output (for [x input
                     y input
                     z input
                     :when (= (+ x y z) 2020)]
                 [x y z])]
    (->> output
         first
         (apply *)
         print)))
