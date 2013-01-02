(ns clearley.npda
  (:refer-clojure :exclude [reduce reductions peek pop])
  (require [clearley.collections.ordered-map :as om]
           [clearley.collections.ordered-set :as os]
           [clojure.core :as core])
  (use clearley.utils))

;(defrecord Reduction [rule count])

(defprotocol Node
  (shift [self input])
  (reduce [self output])
  (reductions [self]))

; Things for a non-deterministic pushdown automaton (an NPDA).
; An NDPA state is a (node, stack, output-stream) tuple.
;
; The input stream is the same for all active states. Each node may 'shift'
; by consuming input and returning a new node. This node is placed
; on the stack for the new state.
; Two states that shift into the same state will have their stacks
; unified--the stack becomes a directed acyclic graph instead of a tree.
; The output streams remain unambiguous, though fragments are shared
; to save space and reduce asymptotic memory.
;
; A set of all states processed for a given input token is called a 'chart'.
; The automaton works by repeatedly processing charts.
;
; Each node may also return some number of 'reductions'.
; A reduction is a (output-state: state, pop-count: int) pair.
; The stack is popped pop-count times,
; revealing some number of underlying states. The output-state is written to out,
; and (reduce % output-state) is called on the underlying states. The new state
; (reduced state) is returned and pushed to the stack. This new state
; may itself have some number of reductions. During the reduce step,
; if any (state, stack) pair is already present in the current chart,
; it is discarded entirely--
; ambiguous output streams are not supported.

(defprotocol State
  (shift-state [self input-token input]) ; Simultaneously push a node and emit output.
  (spin-state [self]) ; Emits a seq of reduced states
  (peek [self])
  (pop [self]) ; Returns a seq of states
  (stream [self])
  (rstream [self]) ; private accessor
  (astack [self])
  (unify [self other-state]))

(defn popone [state]
  (first (pop state)))

; node: the item set for this state
; ostream: the partial output associated with this state
; --a full (nondeterministic) output stream can be got by concating
;  all the ostreams on the stack, top to bottom, then reversing them
; stack: the origin state (will be null for the seed item at index 0)
(defrecord AState [node ostream stack]
  State
  (shift-state [self input-token input]
    (when-let [n (shift node input-token)]
      (AState. n (list input) (list self))))
  ; TODO holy cow, this is ugly. there must be a more elegant way
  (spin-state [self]
    (mapcat
      (fn [[item match-count]]
        (let [new-states (nth (iterate #(mapcat pop %) [self]) match-count)]
          (mapcat (fn [new-state]
                    (map (fn [node]
                           (AState. node (list item) (list new-state)))
                         (reduce (peek new-state) item)))
                  new-states)))
      (reductions node)))
  (peek [_] node)
  (pop [_]
    (map (fn [prev-state]
           (AState. (peek prev-state)
                    (concat ostream (rstream prev-state))
                    (astack prev-state)))
         stack))
  (stream [self] (reverse ostream))
  (rstream [_] ostream)
  (astack [_] stack)
  (unify [self ostate]
    (let [on (peek ostate)
          os (astack ostate)
          oo (rstream ostate)]
      ; doall prevents lazy stack explosions with very large numbers of states
      ; for some reason
      (AState. node ostream (doall (concat stack os)))))
  PStrable
  (pstr [self]
    (with-out-str
      (println "State" (hash self))
      (print "Stack tops" (if (seq stack)
                            (separate-str " " (map hash stack))
                            "(none)"))
      (println)
      (print (pstr node)))))

(defn state [node] (AState. node [] '()))

; ===
; Charts
; Implementing a nondeterministic automaton.
; ===

; states: an ordered set
(defrecord Chart [states]
  PStrable
  (pstr [self]
    (separate-str "---\n" (map pstr (os/vec states)))))

(defn initial-chart [node]
  (Chart. (os/ordered-set (state node))))

; process states for a single chart
(defn reduce-chart [chart]
  (loop [c chart, dot 0]
    (if-let [set (os/get (:states c) dot)]
      (do
        (recur (core/reduce (fn [chart state]
                         (update chart :states #(os/conj % state)))
                       c (spin-state set))
               (inc dot)))
      c)))

; consume input and shift to the next chart
(defn shift-chart [chart thetoken thechar]
  (Chart.
    (loop [stack->state om/empty
           states (os/vec (:states chart))]
      (if-let [old-state (first states)]
        (recur
          (if-let [new-state (shift-state old-state thetoken thechar)]
            (let [stack-top (peek new-state)]
              (om/assoc stack->state stack-top
                        (if-let [old-new-state (om/get stack->state stack-top)]
                          (unify old-new-state new-state)
                          new-state)))
            stack->state)
          (rest states))
        (os/into os/empty (om/vals stack->state))))))

; get the chart for an input
(defn process-chart [chart token input]
  (reduce-chart (shift-chart chart token input)))

; Runs the automaton, returning a sequence of charts
(defn run-automaton [initial-node input tokenizer]
  (loop [pos 0
         remaining-input input
         current-chart (initial-chart initial-node)
         charts [current-chart]]
    (if-let [thechar (first remaining-input)]
      (let [next-chart (process-chart current-chart (tokenizer thechar) thechar)
            next-charts (conj charts next-chart)]
        (if (seq (os/vec (:states next-chart)))
          (recur (inc pos) (rest remaining-input) next-chart next-charts)
          ; early termination on failure returning failed charts
          next-charts))
      ; end returning all charts
      charts)))
