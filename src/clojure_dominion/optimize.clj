(ns clojure-dominion.optimize
  (:use clojure-dominion.core)
  (:use clojure.pprint)
  (:use clojure.tools.trace))

(defn count-points
  "Takes all game moves, as returned by clojure.core.play,
   and returns result as a map of name/points values"
  [moves]
  (let [n (->> moves (map :name) distinct count)
        final-moves (take n moves)
        get-result (fn [move]
                     {(:name move) (count-type move :points)})]
    (apply merge (map get-result final-moves))))

(defn count-series-wins
  [result]
  (->> result
       (map (partial apply max-key val))
       (map key)
       frequencies))

(defn play-series
  "Returns result of n games between input strategies as a map
   'name/number of wins'. In case of tie both strategies wins"
  [n & strategies]
  (->> #(count-points (apply play strategies))
       repeatedly
       (take n)
       count-series-wins))

(defn play-balanced-series
  "Returns result of n games once with given input order once with reversed
   order of strategies"
  [n & strategies]
  (merge-with + (apply play-series n strategies)
              (apply play-series n (reverse strategies))))

(defn neighbours
  "Takes vector of strategy params and returns list of lists of neighbour
  values for each param. Filters out negative and bigger then 9 values.
  Example: (neighbours [0 2]) => ((0 1) (1 2 3))"
  [params]
  (->> params
       (map #(map (partial + %) (range -1 2)))
       (map (partial filter #(and (>= % 0) (<= % 9))))))

(defn permutations
  "Takes coll of lists where each list is possible parameter value as returned
  by neighbours fn. Returns all possible combinations of possible parameters values.
  Example: (permutations [[1 2] [3 4]]) => ((1 3) (1 4) (2 3) (2 4))"
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
  "Takes n and input params for strategy. For given input params generates
   permutations of params neighbours and play n balanced series games
   between input params strategy and each generated strategy.
   Returns sorted list of pairs 'strategy name/number of wins'"
  [n input-params]
  (let [candidates (-> input-params neighbours permutations)
        current-startegy (params->strategy input-params)]
    (->> (map params->strategy candidates)
         (map (partial play-balanced-series n current-startegy))
         (map (partial apply max-key val))
         (remove #(= (str input-params) (first %)))
         (sort-by second >))))

(defn best-neighbour-params
  [n params]
  (->> (neighbour-results n params) first key read-string vec))

(defn subsets
  [n items]
  (cond
    (empty? items) '()
    (= n 1) (map list items)
    :else (concat (map
                    (partial cons (first items))
                    (subsets (dec n) (rest items)))
                  (subsets n (rest items)))))

(defn results->params
  [result]
  (map (comp vec read-string first) result))

(defn find-duplicate
  ([max coll]
    (find-duplicate max coll []))
  ([max coll elements]
    (let [x (first coll)]
      (if (or (some #{x} elements)
              (= (count elements) max))
        [x (conj elements x)]
        (recur max (rest coll) (conj elements x))))))

(defn optimize-iteratively
  [max-steps params f]
  (->> params
       (iterate f)
       (find-duplicate max-steps)))

;(optimize-iteratively 10 [5 4 3 2]
;                      (partial best-neighbour-params 10))


(defn best-tournament-params
  [neighbour-plays tournament-plays tournament-players params]
  (->> (neighbour-results neighbour-plays params)
       (take tournament-players)
       results->params
       (cons params)
       (map params->strategy)
       (subsets 2)
       (map (partial apply play-balanced-series tournament-plays))
       count-series-wins
       (sort-by val >)
       ((comp read-string key first))))

;(optimize-iteratively 5 [5 5 3 3]
;                      (partial best-tournament-params 5 5 3))



















