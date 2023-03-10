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

(def black-tree
  (->> "black-tree.json"
       io/resource
       slurp
       from-json))

(def black-info
  (->> "black-info.json"
       io/resource
       slurp
       json/read-str))

(def white-tree
  (->> "white-tree.json"
       io/resource
       slurp
       from-json))

(def white-info
  (->> "white-info.json"
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

  (let [fens
        (->> ["c2c4" "c7c6" "b1c3" "d7d5" "c4d5" "c6d5" "d2d4"]
             (get-in-tree black-tree)
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
                      (get-in black-info [b "posStats" "rgScore"])
                      (/ (get-in black-info [b "posStats" "lichessStats" "blackWins" "agg"])
                       (get-in black-info [b "posStats" "lichessStats" "whiteWins" "agg"]))]) fens)
    )



  (let [fens
        (->> ["g1f3" "d7d5"]
             (get-in-tree black-tree)
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
                      (get-in black-info [b "posStats" "rgScore"])
                      (/ (get-in black-info [b "posStats" "lichessStats" "whiteWins" "agg"])
                       (get-in black-info [b "posStats" "lichessStats" "blackWins" "agg"]))]) fens)
    )

  ;; thinking will take an hour
  (def p
    (run-timed-rep-gen
     {:color-l          "white"
      :masters-p        false
      :init-resp-prob   0.003
      :asym-resp-prob   0.4
      :min-prob-agg     0.003
      :export-tree-path "./resources/white-tree.json"
      :export-info-path "./resources/white-info.json"
      :export-pgn-path  "./resources/white.pgn"}))

  (destroy-tree p)


  ;; calculating time taken for whole tree
  (->>
   (map (fn [x y] [x y])
        [99 42 13 12 25 27 15 7]
        [0.535 0.622 0.610 0.923 0.486 0.231 0.698 0.637])
   reverse
   (reduce (fn [agg [s p]] (/ (+ agg s) p)) 0)
   (#(/ % 60)))

;; 7 * 0.637 to get the time taken for all leaves
;; (^ + 15) * 0.698


  )


