(ns clojure-dominion.core)

(def base-cards-stats
  "Cards statistics as a map of cards type (:copper etc) to stats map"
  {:copper   {:cost 0 :value 1 :points 0}
   :silver   {:cost 3 :value 2 :points 0}
   :gold     {:cost 6 :value 3 :points 0}
   :estate   {:cost 1 :value 0 :points 1}
   :duchy    {:cost 5 :value 0 :points 3}
   :province {:cost 8 :value 0 :points 6}})

(def init-board
  "Initial cards on board"
  {:copper   60
   :silver   40
   :gold     30
   :estate   24
   :duchy    8
   :province 8})

(def init-cards
  "Inital cards for player"
  {:copper 7
   :estate 3})

(def init-player
  "Init player state"
  {:cards     init-cards
   :discarded {}})

(defn new-player
  "Creates new player atom. Player consist of :cards :discarded :name and :action"
  ([[name action]]
    (new-player name action))
  ([name action]
    (atom (assoc init-player :name name :action action))))

(defn count-cards
  "Counts number of cards in cards map"
  [cards]
  (if (empty? cards)
    0
    (apply + (vals cards))))

(defn select-card
  "Select random single card from cards as a card map"
  ([cards]
    (select-card (inc (rand-int (count-cards cards))) 0 (seq cards)))
  ([limit acc [[card count] & rest-deck]]
    (when card
      (let [new-acc (+ acc count)]
        (if (>= new-acc limit)
          {card 1}
          (recur limit new-acc rest-deck))))))

(defn select-cards
  "Return n randomily selected cards from given cards"
  ([n cards]
    (select-cards n cards {}))
  ([n cards result]
    (if (zero? n)
      result
      (let [selected (select-card cards)]
        (recur (dec n)
               (merge-with - cards selected)
               (merge-with + result selected))))))

(defn count-of
  "Returns counts of all statistics like :points/:value for given cards."
  [stats stat cards]
  (apply + (map (fn [[card count]] (* (stat (stats card {}) 0) count))
                cards)))

(def count-of-base
  (partial count-of base-cards-stats))

(defn remove-zero-cards
  [cards]
  (into {} (remove (comp zero? val) cards)))

(defn take-from-deck!
  "Take n cards from player cards, ignore discarded cards"
  [n player]
  (let [hand (select-cards n (@player :cards))
        new-cards (merge-with - (@player :cards) hand)]
    (swap! player assoc :cards (remove-zero-cards new-cards))
    hand))

(defn discard-cards!
  "Takes player and multiple card maps and adds them to player discarded cards"
  [player & cards-coll]
  (let [discarded (apply merge-with + (@player :discarded) cards-coll)]
    (swap! player assoc :discarded discarded)))

(defn discarded->cards!
  "Switches player discarded cards to deck, sets discarded map to empty"
  [player]
  (let [all-cards (merge-with + (:cards @player) (:discarded @player))
        _ (println "discarded->cards! all-cards=" all-cards)]
    (swap! player assoc :cards all-cards)
    (swap! player assoc :discarded {})))

(defn has-discarded?
  "Check if player has discarded cards"
  [player]
  (-> (player :discarded) count-cards pos?))

(defn take-cards!
  "Takes next player n cards swichting discarded to deck when deck is empty"
  [n player]
  (let [hand (take-from-deck! n player)
        missing-cards (- n (count-cards hand))]
    (if (and (pos? missing-cards)
             (has-discarded? @player))
      (do (discarded->cards! player)
          (merge-with + hand (take-from-deck! missing-cards player)))
      hand)))

(def take-hand!
  (partial take-cards! 5))

(defn can-buy?
  "Test if with board and money one can buy card"
  [board card money]
  (and (pos? (card board 0))
       (>= money (-> base-cards-stats card :cost))))

(defn paramized-big-money*
  [gold-min-p duchy-max-p silver-min-p estate-max-p]
  (fn [board hand]
    (let [value (count-of-base :value hand)
          provinces (:province board 0)
          _ (println "executed paramized-big-money hand=" hand)]
      (cond
        (can-buy? board :province value) {:province 1}
        (and (can-buy? board :gold value) (>= provinces gold-min-p)) {:gold 1}
        (and (can-buy? board :duchy value) (<= provinces duchy-max-p)) {:duchy 1}
        (and (can-buy? board :silver value) (>= provinces silver-min-p)) {:silver 1}
        (and (can-buy? board :estate value) (<= provinces estate-max-p)) {:estate 1}
        :else {:cannot-buy 1}))))

(def optimized-big-money
  (paramized-big-money* 4 5 2 3))

(defn simple-buy*
  [& prefered]
  (fn [board hand]
    (let [money (count-of-base :value hand)
          buy (first (filter #(can-buy? board % money) prefered))]
      (if buy {buy 1} {}))))

(def province-gold-duchy-silver
  (simple-buy* :province :gold :duchy :silver))

(def province-gold-silver
  (simple-buy* :province :gold :silver))

(defn count-type
  [player type]
  (+ (count-of-base type (:cards player))
     (count-of-base type (:discarded player))))

(defn game-finish?
  [board]
  (zero? (:province @board)))

(defn turn
  "Executes single turn in game and modifies board and player accordingly.
   Returns new player state"
  [board player]
  (let [hand (take-hand! player)
        buy ((:action @player) @board hand)]
    (swap! board (partial merge-with -) buy)
    (discard-cards! player (merge-with + hand buy))))

(defn play
  "Plays game between strategies arguments. Strategy is pair name/action.
   Returns list of all game moves starting from last one. Move is defined
   as new player state after executing his actionis player
   state at given moment of game"
  [& strategies]
  (let [board (atom init-board)
        players (map new-player strategies)]
    (loop [moves '()
           order (cycle players)]
      (if (game-finish? board)
        moves
        (recur (conj moves (turn board (first order))) (rest order))))))
