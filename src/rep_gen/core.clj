(ns rep-gen.core
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]]
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

(defn run-in-tmux
  [cmd]
  (sh
   "bash"
   "-c"
   (str
    cmd
    " 1> /proc/"
    (project-pid)
    "/fd/1 2> /proc/"
    (project-pid)
    "/fd/2")))

(defn run-rep-gen
  [config]
  (run-in-tmux
   (str "cabal run rep-gen -- -c '" (to-config-json config) "'")))


;; figure out how to turn edn into json and send to the following command
;; cabal run rep-gen -- -c <CONFIG>
;; then redirect the stdout and stderr (ostensibly just through tee) to the file descriptors pulled by the session name

(comment
  (run-in-tmux "echo hi")

  (run-rep-gen
   {:color-l  "black"
    :masters-p false})

  )


