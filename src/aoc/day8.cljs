(ns ^:figwheel-no-load aoc.day8
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]))

(def example
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-line [line]
  (let [[i n] (string/split line " ")]
    [i (js/parseInt n 10)]))

(defn parse [input]
  (->> input
       string/split-lines
       (map parse-line)))

(parse example)

(defn exec [state [i n]]
  (case i
    "acc" (-> state
              (update :acc #(+ % n))
              (update :line inc))
    "jmp" (update state :line #(+ % n))
    "nop" (update state :line inc)))

(defn conj-state [states [i n]]
  (conj states (exec (last states) [i n])))

(defn solve [code]
  (let [*lines (atom #{})
        *states (atom [{:acc 0 :line 0}])]
    (loop []
      (let [line (-> @*states last :line)]
        (cond
          (contains? @*lines line) nil
          (>= line (count code)) (pprint @*states)
          :else (do
                  (swap! *lines conj line)
                  (swap! *states conj-state (nth code line))
                  (recur)))))))


(defn solve2 [code]

  (doseq [i (range (count code))
        :when (= (first (nth code i)) "nop")]
    (solve (assoc-in code [i 0] "jmp")))

  (doseq [i (range (count code))
          :when (= (first (nth code i)) "jmp")]
    (solve (assoc-in code [i 0] "nop"))))

(solve2 (parse example))

(go
  (-> "input8"
      xhr/get
      <p!
      parse
      solve2))
