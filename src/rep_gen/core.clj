(ns rep-gen.core
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   ;; [clojure.java.shell :refer [sh]]
   [babashka.process :refer [sh process destroy-tree]]
   [clojure.string :as str]))

(defn from-json [raw]
  (json/read-str raw :key-fn (comp csk/->kebab-case keyword)))

(defn to-config-json
  [config]
  (json/write-str config :key-fn csk/->camelCaseString))

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

(defn project-name
  []
  (last
   (str/split
    (str/trim-newline
     (:out
      (sh "pwd")))
    #"/")))

(defn project-pid
  []
  (str/trim-newline
   (:out
    (sh "tmux" "list-panes" "-t" (project-name) "-F" "#{pane_pid}"))))

(defn stdout-fd
  []
  (str "/proc/" (project-pid) "/fd/1"))

(defn stderr-fd
  []
  (str "/proc/" (project-pid) "/fd/2"))

(defn run-in-project
  [cmd]
  (process
   {:out (stdout-fd) :err (stderr-fd)}
   cmd))

(defn run-rep-gen
  [config]
  (run-in-project
   (str "cabal run rep-gen -- -c '" (to-config-json config) "'")))

(comment

  (def p
    (run-rep-gen
     {:color-l          "black"
      :masters-p        false
      :init-resp-prob   0.003
      :asym-resp-prob   0.4
      :min-prob-agg     0.003
      :export-tree-path "./resources/black-tree.json"
      :export-info-path "./resources/black-info.json"
      :export-pgn-path  "./resources/black.pgn"}))

  (destroy-tree p)


  )


