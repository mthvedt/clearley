(ns clearley.npda
  (:refer-clojure :exclude [peek pop])
  (require [uncore.collections.worm-ordered-map :as om]
           [uncore.collections.worm-ordered-set :as os]
           [uncore.str :as s]
           [clojure.core :as core])
  (use uncore.core))

; For use in print-charts &c.
(defprotocol IPrinting
  (pstr [obj]))

; The NPDA is a nondeterministic stack machine.
; A node may: shift (call a subnode), goto (return a new subnode in place),
; return (return a value and pop this node), or continue (accept a return value
; from a shift). Only return may be a vector (TODO).
(defprotocol Node
  (node-key [self])
  (shift [self input]) ; Accept input and put a new state ont stack
  (bounce [self input])
  (continue [self output]) ; Accept a return value
  (return [self])) ; This state is ready to return

; Things for a non-deterministic pushdown automaton (an NPDA).
; An NDPA state is a (node, stack, output-stream) tuple.
; The output streams are partial, containing only the output emitted while
; that state was on the stack.
;
; The input stream is the same for all active states. Each node may 'shift'
; by consuming input and returning a seq of nodes which are then
; placed on the stack.
; Two states that shift into the same state will have their stacks
; unified--the stack becomes a directed acyclic graph instead of a tree.
; The output streams remain unambiguous, though fragments are shared
; to save space and reduce asymptotic memory.
;
; A set of all states processed for a given input token is called a 'chart'.
; The automaton works by repeatedly processing charts.
;
; Each node may also return a return value. For return value, 
; the stack is popped, revealing some number of underlying states,
; and (continue % output-state) is called on the underlying states.
; Continue produces a new state that is substituted onto the stack.
; may itself have some number of reductions. During the return step,
; if any (state, stack) pair is already present in the current chart,
; it is discarded entirely--
; ambiguous output streams are not supported.

(defprotocol State
  (shift-state [self input-token input]) ; Simultaneously push a node and emit output.
  (spin-state [self]) ; Emits a seq of new states
  (accept-return [self rvalue])
  (state-key [self])
  (peek [self])
  (pop [self]) ; Returns a seq of states
  (stream [self])
  (rstream [self]) ; private accessor
  (prevs [self])
  (unify [self other-state]))

(defn popone [state]
  (first (pop state)))

; node: the node for this state
; my-rstream: the partial output associated with this state
; --a full (nondeterministic) output stream can be got by concating
;  all the rstreams on the stack, top to bottom, then reversing them
; my-prevs: the previous state on the stack (will be null for the seed item at index 0)
(defrecord AState [node my-rstream node->prevs]
  State
  (shift-state [self input-token input]
    (when-let [n (shift node input-token)]
      (AState. n (list input) (om/assoc om/empty node self))))
  (spin-state [self]
    (let [returns (return node)
          underlyings (pop self)]
      (remove nil? (concat
                     (mapcat (fn [return-value]
                               (map #(accept-return % return-value) underlyings))
                             returns)
                     (mapcat (fn [return-value]
                               (map #(when-let [new-node (bounce (peek %)
                                                                 return-value)]
                                       (AState. new-node (list return-value)
                                                (om/assoc om/empty (peek %) %)))
                                    underlyings))
                             returns)))))
  (state-key [self] 
    (cons (node-key node) (map state-key (om/vals node->prevs))))
  (accept-return [_ r-value]
    (when-let [continuation (continue node r-value)]
      (AState. continuation (cons r-value my-rstream) node->prevs)))
  (peek [_] node)
  (pop [_]
    (map (fn [my-prev]
           (AState. (peek my-prev)
                    ; prevents lazy explosions with very large numbers of states
                    ; for some reason
                    (concat my-rstream (rstream my-prev))
                    (prevs my-prev)))
         (om/vals node->prevs)))
  (stream [self] (reverse my-rstream))
  (rstream [_] my-rstream)
  (prevs [_] node->prevs)
  (unify [self ostate]
    (let [on (peek ostate)
          op (prevs ostate)
          new-prevs (reduce #(let [node2 (peek %2)]
                               (om/assoc % node2
                                         (if-let [old-prev (om/get % node2)]
                                           (unify old-prev %2)
                                           %2)))
                            node->prevs (om/vals op))]
      (AState. node my-rstream new-prevs)))
  IPrinting
  (pstr [self]
    (with-out-str
      (println "State" (hexhash (state-key self)))
      (println "Node" (node-key node))
      (print "Stack" (pr-str (state-key self)))
      #_(print "Stack tops" (if (seq (om/vals node->prevs))
                            (s/separate-str " " (map (fn-> state-key hexhash)
                                                     (om/vals node->prevs)))
                            "(none)"))
      (println)
      (print (pstr node)))))

(defn state [node] (AState. node [] om/empty))

; ===
; Charts
; Implementing a nondeterministic automaton.
; ===

(defprotocol Chart
  (get-state [self index])
  (add-state [self state])
  (states [self]))

(defrecord AChart [states]
  Chart
  (get-state [_ index]
    (get states index))
  (add-state [_ new-state]
    (AChart. (conj states new-state)))
  (states [_] states)
  IPrinting
  (pstr [self]
    (with-out-str
      (println "===")
      (if (seq states)
        (print (s/separate-str "---\n" (map pstr states)))
        (print "(empty)\n"))
      (println "==="))))

(def empty-chart (AChart. []))

; process stateunify for a single chart
(defn spin-chart [chart]
  (loop [c chart, dot 0]
    (if-let [set (get-state c dot)]
      (recur (reduce #(add-state % %2) c (spin-state set))
             (inc dot))
      c)))

; consume input and shift to the next chart
(defn shift-chart [chart thetoken thechar]
  (reduce add-state empty-chart
          (loop [node->state om/empty
                 states (states chart)]
            (if-let [old-state (first states)]
              (recur
                (if-let [new-state (shift-state old-state thetoken thechar)]
                  (let [node (peek new-state)]
                    (om/assoc node->state node
                              (if-let [old-new-state
                                       (om/get node->state node)]
                                (unify old-new-state new-state)
                                new-state)))
                  node->state)
                (rest states))
              (om/vals node->state)))))

; get the chart for an input
(defn process-chart [chart token input]
  (spin-chart (shift-chart chart token input)))

(defn initial-chart [node]
  (spin-chart (add-state empty-chart (state node))))

; Laziness knocks the big-O down a notch
; but doesn't get us to best-case O(n^2)--O(1) for CLR(k) grammars
; because we store matches in the chart. Push parsing could be added in the future
; to accomplish this.
(defn run-automaton-helper [input current-chart tokenizer]
  (cons current-chart
        (lazy-seq
          (when-let [thechar (first input)]
            (let [next-chart (process-chart current-chart
                                            (tokenizer thechar) thechar)]
              (if (seq (states next-chart))
                (run-automaton-helper (rest input) next-chart tokenizer)
                (list next-chart))))))) ; Puts the empty chart at the end

; Runs the automaton, returning a sequence of charts
(defn run-automaton [initial-node input tokenizer]
  (run-automaton-helper input (initial-chart initial-node) tokenizer))

; Saved for later
#_(defn fast-run-automaton [initial-node input tokenizer]
  (loop [remaining-input input
         current-chart (initial-chart initial-node)]
    (if-let [thechar (first remaining-input)]
      (let [next-chart (process-chart current-chart (tokenizer thechar) thechar)]
        (if (seq (states next-chart))
          (recur (rest remaining-input) next-chart)
          nil))
      current-chart)))
