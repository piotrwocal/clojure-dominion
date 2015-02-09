(ns functional-patterns.patterns
  (:require [clojure.string :as str])
  (:use clojure.pprint))

; State-Event pattern
(defn update-state [current-state event]
  ;; return new state
  )

(defn end-state [start-state events]
  (reduce update-state start-state events))



; Consequences patterns
(defn consequences [current-state event]
  ;; return a sequence of new events
  )

(defn apply-consequences [current-state event]
  (reduce update-state current-state
          (consequences current-state event)))

(defn recursive-consequences [current-state event]
  (reduce (fn [state event]
            (recursive-consequences
              state (update-state state event)))
          current-state
          (consequences current-state event)))

(defn chain-consequences [initial-state consequence-fns]
  (loop [state initial-state
         fs consequence-fns
         output []]
    (if (seq fs)
      (let [events ((first fs) state)
            new-state (reduce update-state state events)]
        (recur new-state (rest fs) (into output events)))
      output)))



; Map-Reduce pattern
(defn mapper [value]
  ;; return a sequence of [key value] pairs
  )

(defn reducer [[key values]]
  ;; return a sequence of [key value] pairs
  )

(defn shuffle [pairs]
  (reduce (fn [m [k v]]
            (update-in m [k] (fnil conj []) v))
          {} pairs))

(defn mapreduce [inputs]
  (mapcat reducer (shuffle (mapcat mapper inputs))))



; Reduce-Combine pattern
(defn reduce-fn
  ([]
    ;; return initial "identity" value
    )
  ([result input]
    ;; return updated result
    ))

(defn combine-fn
  ([]
    ;; return initial "identity" value
    )
  ([result-1 result-2]
    ;; return merged or combined results
    ))




; Recursive Expansion pattern
(defn recursive-expansion [expander input]
  (let [output (expander input)]
    (if (= output input)
      input
      (recur expander output))))



; Pipeline pattern
(defn large-process [input]
  (-> input
      subprocess-a
      subprocess-b
      subprocess-c))
(defn subprocess-a [data]
  (let [{:keys [alpha beta]} data]
    (-> data
        (assoc :epsilon (compute-epsilon alpha))
        (update-in [:gamma] merge (compute-gamma beta)))))



; Wrapper pattern
(defn wrapper [f]
  (fn [input]
    ;; ... before ...
    (f input)
    ;; ... after ...
    ))

(def final-function
  (-> original-function wrapper-a wrapper-b wrapper-c))



; Token pattern
(defn begin [target & args]
  ;; ... begin operation or create state in target ...
  ;; Return a function:
  (fn []
    ;; ... cease operation or destroy state ...
    ))

; variation of token pattern Clojure watch
(defn add-watch [reference key function]
  ;; attach a watcher to reference
  )
(defn remove-watch [reference key]
  ;; remove the watcher
  )



; Strategy pattern
(defprotocol Strategy
  (step-one [this operation])
  (step-two [this operation])
  (step-three [this operation]))

(defn process [strategy]
  (->> (initialize-operation)
       (step-one strategy)
       (step-two strategy)
       (step-three strategy)))


; multimethod variant
(defmulti extensible-step (fn [strategy input] strategy))
(defn process [input strategy]
  ;; ... common behavior ...
  (extensible-step strategy input)
  ;; ... common behavior ...
  )

(defmethod extensible-step :strategy-one [_ input] ...)
(defmethod extensible-step :strategy-two [_ input] ...)
(defmethod extensible-step :strategy-three [_ input] ...)