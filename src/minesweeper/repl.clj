(ns minesweeper.repl
  (:require [minesweeper.game :as game]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(declare play)

(defn pad
  [n]
  (format 
   "%-3s"
   (format "%2d" n)))

(defn display-cell
  [cell]
  (case cell
    (:? :u) "__|"
    (0 :c)  "   "
    :m      " @ "
    :e      " X "
    (pad cell)))

(defn header
  [cols]
  (apply
   str
   (concat
    ["   "]
    (map pad (range cols))
    ["\n"])))

(defn display
  [board]
  (apply
   str
   (concat
    (header (count board))
    (mapcat
     (fn [idx row]
       (concat
        [(pad idx)]
        (map
         display-cell
         row)
        ["\n"]))
     (range)
     board))))

(defonce ^:private *board*
  (atom nil))

(defn show-
  [board message]
  (println (display board))
  (println message))

(defn show
  [reality]
  (case (game/state reality)
    :lose (show- reality "You lose :(")
    :win (show- reality "You win :)")
    :continue (show- (game/knowledge reality) "Your move...")))

(defn check
  []
  (show @*board*))

(defn play!
  [rows cols mines]
  (reset! *board* (game/new-reality rows cols mines))
  (check))

(defn move!
  [col row]
  (swap! *board* game/move [row col])
  (check))

