(ns minesweeper.game)


;; :m - mine
;; :c - cleared
;; :u - uncleared
;; :e - exploded (end-state)
;;
;; [[:c :c :m]
;;  [:m :m :u]
;;  [:u :m :u]]
;; 
;; 'reality'
;;
;;     |
;;     | 
;;     V
;;
;; 'knowledge'
;;
;; [[ 2  3 :?]
;;  [:? :? :?]
;;  [:? :? :?]]
;;
;; #  - number of neighboring mines for known cell
;; :? - unknown cell


(defn adjacents
  "Indices of cells sharing a side."
  [[i j]]
  [[(dec i) j]   ;; top
   [i (inc j)]   ;; right
   [(inc i) j]   ;; bottom
   [i (dec j)]]) ;; left


(defn corners
  "Indices of cells sharing a corner."
  [[i j]]
  [[(dec i) (dec j)]   ;; top-left
   [(dec i) (inc j)]   ;; top-right   
   [(inc i) (inc j)]   ;; bottom-right
   [(inc i) (dec j)]]) ;; bottom-left


(defn neighbors
  "Indices of all surrounding cells."
  [position]
  (concat (adjacents position) (corners position)))


(defn contiguous
  "Given a board and a position, get a subset of the indices of cells
  of the same type forming a contiguous area with the cell at position."
  ([board position]
   (let [status (get-in board position)]
     (contiguous board position status #{})))
  ([board position status discovered]   
   (if (not= status (get-in board position)) 
     discovered
     (set 
      (reduce
       #(contiguous board %2 status %1)
       (conj discovered position)
       (->> (adjacents position)
            (shuffle)
            (take 2)
            (remove discovered)))))))


(defn index-2d
  [row-length index-1d]
  [(quot index-1d row-length)
   (rem index-1d row-length)])


(defn scatter-mines
  "Randomly generated 1d indices of mines"
  [board-size mine-count]
  (->> (range board-size) shuffle (take mine-count)))


(defn empty-board
  "Empty, unrevealed game board."
  [row-count col-count]
  (vec 
   (repeat 
    row-count
    (vec (repeat col-count :u)))))


(defn set-one
  "Update one position in the game board."
  [state board position]
  (assoc-in board position state))


(defn set-many
  "Update multiple positions in the game board."
  [state board positions]
  (reduce 
   (partial set-one state)
   board
   positions))


(defn new-reality
  "Generate new underlying game board."
  [rows cols num-mines]
  (let [size (* rows cols)
        mines (map
               (partial index-2d cols)
               (scatter-mines size num-mines))
        board (empty-board rows cols)]
    (set-many :m board mines)))


(defn danger
  "Compute the number of mines surrounding a position."
  [reality position]
  (->> (neighbors position)
       (map #(get-in reality %))
       (filter #{:m})
       (count)))


(defn knowledge
  "Generate player's view of the underlying game board."
  [reality]
  (mapv
   (fn [row-index row]
     (mapv
      (fn [col-index cell]
        (if (= cell :c)
          (danger reality [row-index col-index])
          :?))
      (range)
      row))
   (range)
   reality))


(defn state
  "Determine whether the player has won or lost yet."
  [reality]
  (let [cells (flatten reality)]
    (cond
      (some #{:e} cells) :lose
      (every? #{:m :c} cells) :win
      :else :continue)))


(defn move
  "Update the underlying game board based on the player's move
  and the type of cell they chose."
  [reality position]
  (case (get-in reality position)
    :c reality
    :u (set-many :c reality (contiguous reality position))
    :m (set-one :e reality position)))

