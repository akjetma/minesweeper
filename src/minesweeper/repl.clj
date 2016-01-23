(ns minesweeper.repl
  (:require [minesweeper.game :as game]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(defn pad
  [n]
  (format 
   "%-3s"
   (format "%2d" n)))

(defn display-cell
  [cell]
  (case cell
    (:? :u) "[ ]"
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

(defn prompt
  [question]
  (println question)
  (flush)
  (read-line))

(defn end
  [reality message]
  (println (display reality))
  (println message)
  (when (= (prompt "Try again? (y/n)") "y")
    (game)))

(defn continue
  [reality]
  (println (display (game/knowledge reality)))
  (let [input (prompt "Next move [row column] (without the brackets): ")
        position (map #(Integer. %) (string/split input #" "))
        new-reality (game/move reality position)
        cells (flatten new-reality)]
    (cond
      (some #{:e} cells) (end new-reality "You lose :(")
      (every? #{:m :c} cells) (end new-reality "You win :)")
      :else (continue new-reality))))

(defn game
  []
  (continue
   (game/new-reality 10 10 30)))
