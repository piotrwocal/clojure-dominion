(ns clojure-dominion.action
  (:use clojure-dominion.core)
  (:use clojure.pprint)
  (:use clojure.tools.trace))

; http://dominion.diehrstraits.com/?set=All&f=list

(def action-cards-stats
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
  {:free-action 1 :free-buy 1 :virtual-money 0})

(defn filter-actions
  [cards]
  (map key
       (filter (fn [[k v]] (k action-cards-stats))
               cards)))

(defn eval-action-card-cost
  [cards]
  (let [add-actions (filter #(:add-action (% action-cards-stats)) cards)]
    (if (seq add-actions)
      (->> add-actions (sort-by #(:add-card (% action-cards-stats) 0) >) first)
      (->> cards (sort-by #(:cost (% action-cards-stats)) >) first))))

(defn choose-card
  [board player hand state]
  (let [actions (filter-actions hand)]
    (if (and (pos? (:free-action state))
             (seq actions))
      (eval-action-card-cost actions))))

(defn single-action!
  [board player hand state]
  (let [action-card (choose-card board player hand state)]
    (if action-card
      (play-card action-card board player hand state)
      (buy-card board player hand state))))

(defn action-turn
  "TODO !"
  [board player]
  (single-action board player (take-cards! 5 player) init-move-state))

