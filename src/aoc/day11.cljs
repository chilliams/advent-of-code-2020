(ns ^:figwheel-no-load aoc.day11
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr])
  (:import [goog.string StringBuffer]))

(def example
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn get-neighbors [[r c]]
  (for [i (range (dec r) (+ 2 r))
        j (range (dec c) (+ 2 c))
        :when (or (not= i r)
                  (not= j c))]
    [i j]))

(defn next-state [seats rc]
  (let [seat (get-in seats rc)
        neighbors (get-neighbors rc)
        occupied (transduce
                  (comp
                   (map (partial get-in seats))
                   (map #(if (= "#" %) 1 0)))
                  +
                  neighbors)]
    (cond (and (= "L" seat)
               (= 0 occupied)) "#"
          (and (= "#" seat)
               (>= occupied 4)) "L"
          :else seat)))

(defn next-string [input]
  (let [seats (string/split-lines input)
        sb (StringBuffer.)]
    (doseq [r (range (count seats))]
      (doseq [c (range (count (first seats)))]
        (.append sb (next-state seats [r c])))
      (.append sb "\n"))
    (.toString sb)))

(-> example
    next-string
    next-string
    print)

(defn solve [input]
  (loop [input input
         prev nil]
    (if (= input prev)
      input
      (recur (next-string input) input))))

(-> example
    solve
    frequencies
    (get "#"))

(go
  (-> "input11"
      xhr/get
      <p!
      solve
      frequencies
      (get "#")
      pprint))
