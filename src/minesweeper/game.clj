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
  [[i j]]
  [[(dec i) j] [(inc i) j]
   [i (dec j)] [i (inc j)]])

(defn corners
  [[i j]]
  [[(dec i) (dec j)] [(inc i) (dec j)]
   [(dec i) (inc j)] [(inc i) (inc j)]])

(defn neighbors
  [position]
  (conj (adjacents position) (corners position)))

(defn contiguous
  ([board position]
   (let [status (get-in board position)]
     (contiguous board status #{} position)))
  ([board status met position]   
   (if (= status (get-in board position))
     (set 
      (reduce
       (partial contiguous board status)
       (conj met position)
       (remove met (adjacents position))))
     met)))

(defn index-2d
  [row-length index-1d]
  [(quot index-1d row-length)
   (rem index-1d row-length)])

(defn scatter-mines
  [board-size mine-count]
  (->> (range board-size) shuffle (take mine-count)))

(defn empty-board
  [row-count col-count]
  (vec 
   (repeat 
    row-count
    (vec (repeat col-count :u)))))

(defn set-one
  [state board position]
  (assoc-in board position state))

(defn set-many
  [state board positions]
  (reduce 
   (partial set-one state)
   board
   positions))

(defn new-reality
  [rows cols num-mines]
  (let [size (* rows cols)
        mines (map
               (partial index-2d cols)
               (scatter-mines size num-mines))
        board (empty-board rows cols)]
    (set-many :m board mines)))

(defn danger
  [reality position]
  (count
   (filter
    #{:m}
    (map
     (partial get-in reality)
     (neighbors position)))))

(defn knowledge
  [reality]
  (mapv
   (fn [row cells]
     (mapv
      (fn [col cell]
        (if (= cell :c)
          (danger reality [row col])
          :?))
      (range)
      cells))
   (range)
   reality))

(defn move
  [board position]
  (case (get-in board position)
    :c board
    :u (set-many :c board (contiguous board position))
    :m (set-one :e board position)))

