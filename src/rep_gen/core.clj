(ns rep-gen.core
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.data.json :as json]
   [clojure.java.io :as io]))


(defn from-json [raw]
  (json/read-str raw :key-fn (comp csk/->kebab-case keyword)))

(defn get-in-tree
  [tree moves]
  (reduce (fn [mt move] (->> mt :node-responses (filter #(= move (first %))) first second)) tree moves))



(def move-tree
  (->> "move-tree.json"
       io/resource
       slurp
       from-json))

(def pos-info
  (->> "pos-info.json"
       io/resource
       slurp
       json/read-str))

(comment
  )
