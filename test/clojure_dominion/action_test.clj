(ns clojure-dominion.action-test
  (:use clojure-dominion.core)
  (:use clojure-dominion.action)
  (:use [clojure.test :only [deftest is are]]))

(deftest test-filter-actions
  (are [result cards]
    (=  result (filter-actions cards))
    {} {}
    {} init-cards
    {:village 1 :cellar 1} {:village 1 :cellar 1 :gold 1}
    {:cellar 1} {:village 0 :cellar 1 :gold 1}
    {} {:cellar 0}))

(deftest test-single-action
  (are [hand init-player result-player]
    (= (single-action (atom init-board) (atom init-player) hand init-move-state)
       result-player)

    {} init-player
    init-player

    {:village 1 :gold 1} {:cards {:estate 2} :discarded {}}
    {:cards {:estate 1} :discarded {:village 1, :gold 1, :estate 1}}

    {:village 2 :gold 1} {:cards {:estate 2} :discarded {}}
    {:cards {} :discarded {:village 2, :gold 1, :estate 2}}

    {:village 1 :silver 1} {:cards {:village 1} :discarded {:gold 1}}
    {:cards {} :discarded {:village 2, :gold 1, :silver 1}}

    {:smithy 1 :gold 1} {:cards {:estate 4} :discarded {}}
    {:cards {:estate 1} :discarded {:smithy 1, :gold 1, :estate 3}}

    {:smithy 1} {:cards {:village 1 :estate 2} :discarded {}}
    {:cards {} :discarded {:smithy 1, :village 1 :estate 2}}

    {:village 1 :smithy 1} {:cards {:estate 5} :discarded {}}
    {:cards {:estate 1} :discarded {:smithy 1, :village 1 :estate 4}}

    {:village 1 :smithy 1} {:cards {:smithy 2 :estate 1} :discarded {:silver 5}}
    {:cards {:silver 1} :discarded {:estate 1 :silver 4 :smithy 3 :village 1}}

    {:chancellor 1 :silver 1} {:cards {:copper 2} :discarded {:gold 1}}
    {:cards {:copper 2 :gold 1} :discarded {:chancellor 1 :silver 1}}

    {:chancellor 1 :silver 1} {:cards {:gold 1} :discarded {:copper 2}}
    {:cards {:gold 1} :discarded {:copper 2 :chancellor 1 :silver 1}}

    {:cellar 1 :estate 2} {:cards {:gold 1} :discarded {:gold 2}}
    {:cards {:gold 1} :discarded {:gold 2 :estate 2 :cellar 1}}

    {:cellar 3} {:cards {:gold 2} :discarded {}}
    {:cards {} :discarded {:gold 2 :cellar 3}}

    ; not optimal example of cellar
    {:village 1 :smithy 1 :cellar 1} {:cards {:estate 4} :discarded {:gold 5}}
    {:cards {:gold 4} :discarded {:cellar 1 :estate 4 :gold 1 :smithy 1 :village 1}}
    ))










