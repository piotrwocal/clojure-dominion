(ns clojure-dominion.optimize
  (:use clojure-dominion.core)
  (:use clojure.pprint)
  (:use clojure.tools.trace))

(defn count-points
  [moves]
  (let [n (->> moves (map :name) distinct count)
        final-moves (take n moves)
        get-result (fn [move]
                        {(:name move) (count-type move :points)})]
    (apply merge (map get-result final-moves))))

(defn play-series
  [n & strategies]
  (->> #(count-points (apply play strategies))
       repeatedly
       (take n)
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
  [n current-params]
  (let [candidates (-> current-params neighbours permutations)
        current-startegy (params->strategy current-params)]
    (->> (map params->strategy candidates)
         (map (partial play-balanced-series n current-startegy))
         (map (partial apply max-key val))
         (remove #(= (str current-params) (first %)))
         (sort-by second >))))

(defn next-best-params
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

















