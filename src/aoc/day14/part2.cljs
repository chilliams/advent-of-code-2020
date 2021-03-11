(ns ^:figwheel-no-load aoc.day14.part2
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]
   [goog.string :as gstring])
  (:import
   [goog.math Long]
   [goog.string StringBuffer]))

(def example
  "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
")

(defn parse [input]
  (for [[_ mask address value]
        (re-seq #"mask = (.*)|mem\[(.*)\] = (.*)" input)]
    (if mask
      {:mask mask}
      {:address (-> address
                    (Long/fromString 10)
                    (.toString 2)
                    (gstring/padNumber 36))
       :value (Long/fromString value 10)})))

(defn apply-mask [mask address]
  (apply
   str
   (for [i (range 36)
         :let [m (nth mask i)
               a (nth address i)]]
     (cond
       (= "0" m) a
       (= "1" m) "1"
       (= "X" m) "X"))))

(defn apply-masks [steps]
  (let [*s (atom '())
        *m (atom nil)]
    (doseq [{:keys [mask address value]} steps]
      (if mask
        (reset! *m mask)
        (swap! *s conj {:address (apply-mask @*m address)
                        :value value})))
    @*s))

(defn addresses [a]
  (if (.includes a "X")
    (concat
     (addresses (.replace a "X" "0"))
     (addresses (.replace a "X" "1")))
    [(js/parseInt a 2)]))

(defn map-address [o]
  (update o :address addresses))

(defn map-addresses [s]
  (map map-address s))

(defn remove-seen [steps]
  (let [*s (atom '())
        *seen (atom #{})]
    (doseq [{:keys [address value]} steps
            :let [seen @*seen]]
      (swap! *s conj {:address (filter #(not (contains? seen %)) address)
                      :value value})
      (swap! *seen #(apply conj % address)))
    @*s))

(defn val [{:keys [address value]}]
  (* (count address) value))

(defn total [steps]
  (->> steps (map val) (reduce +)))

(defn solve [input]
  (->> input
       parse
       apply-masks
       map-addresses
       remove-seen
       total))

(solve example)

(go
  (-> "input14"
      xhr/get
      <p!
      solve
      pprint))
