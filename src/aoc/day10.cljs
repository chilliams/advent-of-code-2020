(ns ^:figwheel-no-load aoc.day10
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
  "16
10
15
5
1
11
7
19
6
12
4")

(def example2
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(defn sorted-seq [input]
  (->> (string/split-lines input)
       (map #(js/parseInt % 10))
       sort))

(defn solve [input]
  (let [s (sorted-seq input)
        diffs (->> (conj s 0)
                   (interleave s)
                   (partition 2)
                   (map #(apply - %)))
        ones (->> diffs
                  (filter #(= 1 %))
                  count)
        threes (->> diffs
                    (filter #(= 3 %))
                    count
                    inc)]
    (* ones threes)))

(solve example)

(go
  (-> "input10"
      xhr/get
      <p!
      solve
      pprint))

(def solve2
  (memoize
   (fn [seq]
     (cond
       (< (count seq) 1) (throw (js/Error. "why"))
       (= (count seq) 1) 1
       :else
       (let [this (first seq)]
         (+ (if (and
                 (>= (count seq) 2)
                 (>= (+ 3 this) (nth seq 1)))
              (solve2 (drop 1 seq))
              0)
            (if (and
                 (>= (count seq) 3)
                 (>= (+ 3 this) (nth seq 2)))
              (solve2 (drop 2 seq))
              0)
            (if (and
                 (>= (count seq) 4)
                 (>= (+ 3 this) (nth seq 3)))
              (solve2 (drop 3 seq))
              0)))))))

(<= 1 nil)

(solve2 '(10 11 12 15 16 19))
(solve2 '(16 19))

(->> example
     sorted-seq
     solve2)

(-> example2
    sorted-seq
    (conj 0)
    solve2)

(go
  (-> "input10"
      xhr/get
      <p!
      sorted-seq
      (conj 0)
      solve2
      pprint))
