(ns clojure-dominion.action
  (:use clojure-dominion.core)
  (:use clojure.pprint)
  (:use clojure.tools.trace))

; http://dominion.diehrstraits.com/?set=All&f=list

(def action-stats
  "Action cards statistics as a map of cards type (:copper etc) to stats map"
  {:cellar       {:cost 2 :add-action 1}
   :chapel       {:cost 2}
   :moat         {:cost 3 :add-card 2}
   :chancellor   {:cost 3 :add-money 2}
   :village      {:cost 3 :add-action 2 :add-card 1}
   :woodcutter   {:cost 3 :add-money 2 :add-buy 1}
   :workshop     {:cost 3}
   :bureaucat    {:cost 4}
   :feast        {:cost 4}
   :militia      {:cost 4 :add-money 2}
   :moneylender  {:cost 4}
   :remodel      {:cost 4}
   :smithy       {:cost 4 :add-card 3}
   :spy          {:cost 4 :add-action 1 :add-card 1}
   :thief        {:cost 4}
   :throne-room  {:cost 4}
   :council-room {:cost 5 :add-card 4 :add-buy 1}
   :festival     {:cost 5 :add-action 2 :add-buy 1 :add-money 2}
   :labolatory   {:cost 5 :add-action 1 :add-card 2}
   :library      {:cost 5}
   :market       {:cost 5 :add-action 1 :add-card 1 :add-buy 1 :add-money 1}
   :mine         {:cost 5}
   :witch        {:cost 5 :add-card 2}
   :adventurer   {:cost 6}
   })

(def init-move-state
  {:free-action 1 :free-buy 1 :virtual-money 0 :played []})

(defn filter-actions
  "Takes cards and returns action cards for which cards count is positive"
  [cards]
  (into {} (filter (fn [[k _]] (k action-stats))
                   (filter (comp pos? val) cards))))

(defn eval-actions
  "Takes seq of action cards as keywords and returns next cards to play.
   Implementation sorts cards due to add-action and cost value."
  [cards]
  (let [add-action-cards (filter #(:add-action (action-stats %)) cards)]
    (if (seq add-action-cards)
      (->> add-action-cards (sort-by #(:add-card (action-stats %) 0) >))
      (->> cards (sort-by #(:cost (action-stats %)) >)))))

(defn choose-action
  "Returns selected card to play from hand as keyword"
  [board player hand state]
  (let [actions (map key (filter-actions hand))]
    (if (and (pos? (:free-action state 0))
             (seq actions))
      (first (eval-actions actions)))))

(def single-action)

; >>> play-card multimethod
(defmulti play-card
          "Plays single card according to rules modifying accordingly board/player/state."
          (fn [card board player hand state] card)
          :default nil)

(defmethod play-card nil
  [card board player hand state]
  (single-action board player hand state))

(defmethod play-card :chancellor
  [card board player hand state]
  (do
    (println "play chancelor, hand=" hand " state=" state)
    (discarded->cards! player)
    (single-action board player hand
                   (merge-with + state {:virtual-money 2}))))

(defmethod play-card :village
  [card board player hand state]
  (do
    (println "play village, hand=" hand " state=" state)
    (single-action board player
                   (merge-with + hand (take-cards! 1 player))
                   (merge-with + state {:free-action 2}))))

(defmethod play-card :smithy
  [card board player hand state]
  (do
    (println "play smithy, hand=" hand " state=" state)
    (single-action board player
                   (merge-with + hand (take-cards! 3 player))
                   state)))
; <<< play-card multimethod

; helpers for actions


; play mechanics
(defn buy-card
  [board player hand state]
  (do
    (println "buy-card executed, hand=" hand ", state=" state)
    (single-action board player hand state)))

(defn update-move-state
  [state action]
  (let [new-state (merge-with - state {:free-action 1})]
    (update-in new-state [:played] conj {action 1})))

(defn single-action
  "Executes single player action as play card or single buy.
   Modifies board and player state"
  [board player hand state]
  (let [ _ (println "single-action: player=" player ",hand=" hand ",state=" state)
        action-card (choose-action @board @player hand state)]
    (if action-card
      (play-card action-card board player
                 (merge-with - hand {action-card 1})
                 (update-move-state state action-card))
      (if (pos? (:free-buy state 0))
        (buy-card board player hand
                  (merge-with - state {:free-buy 1}))
        (apply discard-cards! player hand (:played state))))))

(defn action-round
  "Executes full player turn, executing action(s) and buy(s)"
  [board player]
  (single-action board player (take-from-deck! 5 player) init-move-state))

