(ns rep-gen.core
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [babashka.process :refer [sh process destroy-tree]]
   [clojure.string :as str]))

(defn from-json [raw]
  (json/read-str raw :key-fn (comp csk/->kebab-case keyword)))

  (defn clojurize-map
    [x]
    (cond
      (seq? x) (mapv clojurize-map x)
      (map? x) (into {} (map (fn [[k v]] [(csk/->kebab-case-keyword k) (clojurize-map v)])) x)
      :else x))

(defn fetch-tree
  [f-name]
  (->> f-name
       io/resource
       slurp
       from-json))

(defn fetch-info
  [f-name]
  (->> f-name
       io/resource
       slurp
       json/read-str
       (into {} (map (fn [[fen info]] [fen (clojurize-map info)])))))

(defn to-config-json
  [config]
  (json/write-str config :key-fn csk/->camelCaseString))

(defn get-in-tree
  [tree moves]
  (reduce (fn [mt move] (->> mt :node-responses (filter #(= move (first %))) first second)) tree moves))

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

(defn run-in-project
  [cmd]
  (process
   {:out (str "/proc/" (project-pid) "/fd/1")
    :err (str "/proc/" (project-pid) "/fd/2")}
   cmd))

(defn run-rep-gen
  [config]
  (run-in-project
   (str "cabal run rep-gen -- -c '" (to-config-json config) "'")))

(defn run-timed-rep-gen
  [config]
  (run-in-project
   (str "time cabal run rep-gen -- -c '" (to-config-json config) "'")))

(comment

  (let [move-tree (fetch-tree "white-tree.json")
        pos-info (fetch-info "white-info.json")
        fens
        (->> ["g1f3" "d7d5"]
             (get-in-tree move-tree)
             ;; :node-fen
             :node-responses
             (map (fn [[a b]] [a (:node-fen b)]))
             #_#_
             :node-responses
             (map second)
             #_
             (map #(dissoc % :node-responses)))]
    (map (fn [[a b]] [a
                      #_
                      (get-in pos-info [b "posStats" "rgScore"])
                      (/ (get-in pos-info [b "posStats" "lichessStats" "whiteWins" "agg"])
                       (get-in pos-info [b "posStats" "lichessStats" "blackWins" "agg"]))]) fens)
    )

  (def p
    (run-timed-rep-gen
     {:color-l          "black"
      :masters-p        false
      :min-prob-agg     0.01
      :export-tree-path "./resources/black-tree.json"
      :export-info-path "./resources/black-info.json"
      :export-pgn-path  "./resources/black.pgn"}))

  (def p
    (run-timed-rep-gen
     {:color-l          "white"
      :masters-p        false
      :min-prob-agg     0.01
      :export-tree-path "./resources/white-tree.json"
      :export-info-path "./resources/white-info.json"
      :export-pgn-path  "./resources/white.pgn"}))

  (destroy-tree p)

  )


