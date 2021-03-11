(ns ^:figwheel-no-load aoc.day14
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr])
  (:import
   [goog.math Long]
   [goog.string StringBuffer]))

(def example
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
")

(defn and-mask [s]
  (-> s
      (.replaceAll "X" 1)
      (Long/fromString 2)))

(defn or-mask [s]
  (-> s
      (.replaceAll "X" 0)
      (Long/fromString 2)))

(defn mask-fn [s]
  (let [a (and-mask s)
        o (or-mask s)]
    (fn [n]
      (-> n
          (.and a)
          (.or o)))))

(defn parse [input]
  (for [[_ mask address value]
        (re-seq #"mask = (.*)|mem\[(.*)\] = (.*)" input)]
    (if mask
      {:mask mask}
      {:address (js/parseInt address 10)
       :value (Long/fromString value 10)})))

(defn solve [steps]
  (let [*s (atom {})
        *m (atom nil)]
    (doseq [{:keys [mask address value]} steps]
      (if mask
        (reset! *m (mask-fn mask))
        (swap! *s conj {address (@*m value)})))
    (->> @*s vals (reduce (fn [a b] (.add a b)) (Long/getZero)))))

(->> example
     parse
     solve)

(go
  (-> "input14"
      xhr/get
      <p!
      parse
      solve
      pprint))
