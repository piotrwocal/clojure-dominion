(ns clojure-dominion.game-test
  (:use clojure-dominion.game)
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
    3 {:cooper 1 :silver 2} {:cooper 1 :silver 2}
    10 {:cooper 2} {:cooper 2}
    10 {} {}
    0 {:cooper 1} {}))

(def test-cards
  {:copper 1 :silver 2 :gold 3 :estate 1 :duchy 2})

(deftest test-count-of
  (are [n stats cards]
    (= n (count-of stats cards))
    7 :value init-cards
    3 :points init-cards
    14 :value test-cards
    7 :points test-cards))

(deftest test-optimized-big-money
  (are [card board hand]
    (= card (optimized-big-money board hand))
    {:province 1} init-board {:copper 8}
    {:gold 1} init-board {:copper 6}
    {:duchy 1} {:province 3 :gold 1 :duchy 1} {:copper 6}
    {:silver 1} init-board {:copper 5}
    {:silver 1} init-board {:copper 3}
    {:silver 1} {:province 3 :gold 0 :duchy 1
                 :silver   1 :estate 1} {:copper 3}
    {:estate 1} {:province 2 :gold 0 :duchy 1
                 :silver   1 :estate 1} {:copper 3}
    {} init-board {:copper 2}))

; TODO: test-take-hand!









