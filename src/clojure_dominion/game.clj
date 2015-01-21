(ns clojure-dominion.game
  (:use clojure.pprint))

(def cards-stats
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
  {:cards     init-cards
   :discarded {}})

(defn new-player
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
        (recur
          (dec n)
          (merge-with - cards selected)
          (merge-with + result selected))))))

(defn count-of
  "Returns counts of all statistics like :points/:value for given cards."
  [stat cards]
  (apply +
         (map
           (fn [[card count]]
             (* (stat (cards-stats card)) count))
           cards)))

(defn take-cards!
  "Take n cards from player cards, ignore discarded cards"
  [n player]
  (let [hand (select-cards n (@player :cards))
        new-cards (merge-with - (@player :cards) hand)]
    (swap! player assoc :cards new-cards)
    hand))

(defn discard-cards!
  "Takes player and multiple card maps and adds them to player discarded cards"
  [player & cards-coll]
  (let [discarded (apply merge-with + (@player :discarded) cards-coll)]
    (swap! player assoc :discarded discarded)))

(defn discarded->cards!
  "Switches player discarded cards to current, sets discarded map to empty"
  [player]
  (swap! player assoc :cards (@player :discarded))
  (swap! player assoc :discarded {}))

(defn has-discarded?
  "Check if player has discarded cards"
  [player]
  (-> (player :discarded) count-cards pos?))

(defn take-hand!
  "Takes next player hand swichting discarded to current cards when current empty"
  [player]
  (let [hand (take-cards! 5 player)
        missing-cards (- 5 (count-cards hand))]
    (if (and (pos? missing-cards)
             (has-discarded? @player))
      (do
        (discarded->cards! player)
        (merge-with + hand (take-cards! missing-cards player)))
      hand)))

(defn can-buy?
  "Test if with board and money one can buy card"
  [board card money]
  (and (pos? (card board))
       (>= money (-> cards-stats card :cost))))

(defn optimized-big-money
  [board hand]
  (let [value (count-of :value hand)
        provinces (:province board 0)]
    (cond
      (can-buy? board :province value) {:province 1}
      (and (can-buy? board :gold value) (>= provinces 4)) {:gold 1}
      (and (can-buy? board :duchy value) (<= provinces 5)) {:duchy 1}
      (and (can-buy? board :silver value) (>= provinces 2)) {:silver 1}
      (and (can-buy? board :estate value) (<= provinces 3)) {:estate 1}
      :else {})))

(defn paramized-big-money*
  [gold-min-p duchy-max-p silver-min-p estate-max-p]
  (fn [board hand]
    (let [value (count-of :value hand)
          provinces (:province board 0)]
      (cond
        (can-buy? board :province value) {:province 1}
        (and (can-buy? board :gold value) (>= provinces gold-min-p)) {:gold 1}
        (and (can-buy? board :duchy value) (<= provinces duchy-max-p)) {:duchy 1}
        (and (can-buy? board :silver value) (>= provinces silver-min-p)) {:silver 1}
        (and (can-buy? board :estate value) (<= provinces estate-max-p)) {:estate 1}
        :else {}))))

(defn simple-buy*
  [& prefered]
  (fn [board hand]
    (let [money (count-of :value hand)
          buy (first (filter #(can-buy? board % money) prefered))]
      (if buy {buy 1} {}))))

(def province-gold-duchy-silver
  (simple-buy* :province :gold :duchy :silver))

(def province-gold-silver
  (simple-buy* :province :gold :silver))

(defn count-type
  [player type]
  (+ (count-of type (:cards player))
     (count-of type (:discarded player))))

(defn game-finish?
  [board]
  (zero? (:province @board)))

(defn turn
  "Executes single turn in game for board, player and given action.
   Returns new player state"
  [board player]
  (let [hand (take-hand! player)
        buy ((:action @player) @board hand)]
    (swap! board (partial merge-with -) buy)
    (discard-cards! player (merge-with + hand buy))))

(defn monitoring-turn
  "Executes single turn in game for board, player and given action.
   Returns new player state"
  [board player]
  (let [hand (take-hand! player)
        buy ((:action @player) @board hand)
        board (swap! board (partial merge-with -) buy)
        discarded (discard-cards! player (merge-with + hand buy))]
    (do
      (println "----------")
      (pprint (:name @player))
      (pprint (:cards @player))
      (pprint hand)
      (pprint buy)
      (pprint (:discarded @player))
      (flush)
      discarded)))

(defn play
  [& strategies]
  (let [board (atom init-board)
        players (map new-player strategies)]
    (loop [moves '()
           order (cycle players)]
      (if (game-finish? board)
        moves
        (recur (conj moves (turn board (first order))) (rest order))))))

(defn take-result [[move-1 move-2 & _]]
  {(:name move-1) (count-type move-1 :points)
   (:name move-2) (count-type move-2 :points)})

(defn play-series
  [n & strategies]
  (->> #(take-result (apply play strategies))
       repeatedly
       ((partial take n))
       (map (partial apply max-key val))
       (map key)
       frequencies))

(defn play-balanced-series
  [n & strategies]
  (merge-with + (apply play-series n strategies)
              (apply play-series n (reverse strategies))))

(defn neighbours
  [params]
  (->> params
       (map #(map (partial + %) (range -1 2)))
       (map (partial filter #(and (>= % 0) (<= % 9))))))

(defn permutations
  [[params & params-seq]]
  (if (nil? params-seq)
    (map vector params)
    (mapcat
      #(map (partial cons %) (permutations params-seq))
      params)))

(defn params->strategy
  [params]
  [(str params) (apply paramized-big-money* params)])

(defn neighbour-results
  [n params]
  (let [best-params params
        candidates (-> best-params neighbours permutations)
        best-strategy (params->strategy best-params)
        candidates-strategies (map params->strategy candidates)]
    (->> candidates-strategies
         (map (partial play-balanced-series n best-strategy))
         (map (partial apply max-key val))
         (remove #(= (str best-params) (first %)))
         (sort-by second >))))

(defn next-best-params
  [n params]
  (->> (neighbour-results n params) first key read-string vec))

(defn subsets
  [n items]
  (cond
    (= n 0) '(())
    (empty? items) '()
    :else (concat (map
                    (partial cons (first items))
                    (subsets (dec n) (rest items)))
                  (subsets n (rest items)))))

(defn results->params
  [results]
  (map (comp vec read-string first) results))

(->> (neighbour-results 10 [5 7 2 1])
     (take 3)
     results->params
     (map params->strategy)
     (subsets 2)
     (map (partial apply play-balanced-series 10))
     pprint)


;------------

















