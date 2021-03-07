(ns ^:figwheel-no-load aoc.day3
  (:require
   [clojure.string :as string]
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [goog.labs.net.xhr :as xhr]))

(def example1
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn solve
  ([input-str] (solve input-str [3 1]))
  ([input-str path]
   (let [input (string/split-lines input-str)
         width (count (first input))
         [right down] path]
     (loop [pos path
            r 0]
       (let [[a b] pos
             val (get-in input [b a])
             next [(-> a (+ right) (mod width))
                   (+ b down)]]
         (cond
           (nil? val) r
           (= "#" val) (recur next (inc r))
           :else (recur next r)))))))

(solve example1)

(go
  (let [input (-> "input3"
                  xhr/get
                  <p!)
        r (solve input)]
    (print r)))

;; part 2
(def to-check
  [[1 1]
   [3 1]
   [5 1]
   [7 1]
   [1 2]])

(->> to-check
     (map (partial solve example1))
     (reduce *))

(go
  (let [input (-> "input3"
                  xhr/get
                  <p!)]
    (->> to-check
         (map (partial solve input))
         (reduce *)
         print)))
