(ns clojure-dominion.core-test
  (:use clojure-dominion.core)
  (:use [clojure.test :only [deftest is are]]))

(deftest test-count-cards
  (are [result cards]
    (= (count-cards cards) result)
    10 init-cards
    6 {:copper 1, :silver 2, :gold 3}
    0 {}))

(deftest test-select-cards
  (are [n cards result]
    (= (select-cards n cards) result)
    1 {:copper 2} {:copper 1}
    3 {:copper 1 :silver 2} {:copper 1 :silver 2}
    10 {:copper 2} {:copper 2}
    10 {} {}
    0 {:copper 1} {}))

(def test-cards
  {:copper 1 :silver 2 :gold 3 :estate 1 :duchy 2})

(deftest test-count-of
  (are [n stats cards]
    (= n (count-of-base stats cards))
    7 :value init-cards
    3 :points init-cards
    14 :value test-cards
    7 :points test-cards))

(deftest test-take-hand!
  (let [player (atom init-player)
        hand (take-hand! player)]
    (is (= init-cards
           (merge-with + hand (:cards @player))))
    (is (= init-cards
           (merge-with + hand (take-hand! player))))
    (is (empty? (take-hand! player)))
    (is (zero? (count-cards (:cards @player))))
    (is (zero? (count-cards (:discarded @player))))))

(deftest test-can-buy?
  (are [can board card money]
    (= can (can-buy? board card money))
    true init-board :province 8
    false init-board :province 7
    false init-cards :province 8))

(deftest test-optimized-big-money)
(are [card money board]
  (= card (optimized-big-money board money))
  {:province 1} 8 init-board
  {:gold 1} 6 init-board
  {:duchy 1} 6 {:province 3 :gold 1 :duchy 1}
  {:silver 1} 5 init-board
  {:silver 1} 3 init-board
  {:silver 1} 3 {:province 3 :gold 0 :duchy 1 :silver 1 :estate 1}
  {:estate 1} 2 {:province 2 :gold 0 :duchy 1 :silver 1 :estate 1}
  {} 2 init-board)

; TODO: have a look at test.generative lib









