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
  (return [self input])) ; This state is ready to return

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
  (shift-state [self input-token input pos])
  ; Simultaneously push a node and emit output.
  (spin-state [self count input-token]) ; Emits a seq of new states
  (accept-return [self rvalue])
  (state-key [self])
  (position [self])
  (peek [self])
  (pop [self count]) ; Returns a seq of states
  (stack-count [self])
  (rstream [self]) ; private accessor
  (prevs [self])
  (unify [self other-state]))

; node: the node for this state
; my-rstream: the partial output associated with this state
; --a full (nondeterministic) output stream can be got by concating
;  all the rstreams on the stack, top to bottom, then reversing them
;  position: the position fo this state.
; Two states with the same position and node are IDENTICAL.
; There might be a better algorithmic way to do this but this gives us O(n^3)
; (provably!) ; without breaking a sweat.
; my-prevs: the previous state on the stack (will be null for the seed item at index 0
(deftype AState [node my-position my-rstream stack-map]
  State
  (shift-state [self input-token input pos]
    (when-let [n (shift node input-token)]
      (AState. n pos (list input)
               (om/assoc om/empty [(node-key node) (inc my-position)] self))))
  (spin-state [self c input-token] ; c is for when we need to spin multiple times
    (let [returns (return node input-token)
          underlyings (pop self c)]
      (remove nil? (concat
                     (mapcat (fn [return-value]
                               (map #(accept-return % return-value) underlyings))
                             returns)
                     (mapcat (fn [return-value]
                               (map #(when-let [new-node (bounce (peek %)
                                                                 return-value)]
                                       (AState. new-node my-position
                                                (list return-value)
                                                (om/assoc om/empty (state-key %) %)))
                                    underlyings))
                             returns)))))
  (state-key [self] 
    [(node-key node) my-position])
  (accept-return [_ r-value]
    (when-let [continuation (continue node r-value)]
      (AState. continuation my-position (cons r-value my-rstream) stack-map)))
  (peek [_] node)
  (position [_] my-position)
  (pop [_ c]
    (map (fn [my-prev]
           (AState. (peek my-prev)
                    (position my-prev)
                    (concat my-rstream (rstream my-prev))
                    (prevs my-prev)))
         (drop c (om/vals stack-map))))
  (stack-count [_] (om/count stack-map))
  (rstream [_] my-rstream)
  (prevs [_] stack-map) ; TODO rename
  (unify [self ostate]
    ;(prn "Unifying")
    ;(prn (pstr self))
    ;(prn "with")
    ;(prn (pstr ostate))
    (let [op (prevs ostate)
          new-prevs (reduce #(om/assoc % (state-key %2) %2)
                            stack-map (om/vals op))]
      (AState. node my-position my-rstream new-prevs)))
  IPrinting
  (pstr [self]
    (with-out-str
      (println "State" (pr-str (state-key self)))
      (print "Stack tops" (if (seq (om/vals stack-map))
                            (s/separate-str " " (map pr-str
                                                     (-> stack-map om/keys os/vec)))
                            "(none)"))
      (println)
      (print (pstr node)))))

(defn initial-state [node] (AState. node 0 [] om/empty))

; ===
; Charts
; Implementing a nondeterministic automaton.
; ===

(defprotocol Chart
  (get-state [self index])
  (states [self]))

(defrecord AChart [my-states]
  Chart
  (get-state [_ index]
    (get my-states index))
  (states [_] my-states)
  IPrinting
  (pstr [self]
    (with-out-str
      (println "===")
      (if (seq (states self))
        (print (s/separate-str "---\n" (map pstr (states self))))
        (print "(empty)\n"))
      (println "==="))))

(def empty-chart (AChart. []))

(defn process-chart [chart thetoken thechar next-token pos]
  ; Shift and reduce, with bookkeeping making sure all states are processed
  ; Assumption: shifted nodes are always terminal--they cannot come from a reduce
  ;
  ; TODO yeeeeah this is too complicated
  (let [seed-states
        ; Shift-step
        (loop [shifting-states (states chart) seed-states []]
          (if-let [shifting-state (first shifting-states)]
            (recur (rest shifting-states)
                   (if-let [shifted-state (shift-state shifting-state thetoken
                                                       thechar pos)]
                     (conj seed-states shifted-state)
                     seed-states))
            seed-states))]
    ; Reduce step
    (loop [dot 0 state-map om/empty state-queue seed-states]
      (if-let [current-state (get state-queue dot)]
        (let [current-state-key (state-key current-state)
              previous-state? (om/get state-map current-state-key)
              pop-count (if previous-state? (stack-count previous-state?) 0)
              current-state (if previous-state? 
                              (unify previous-state? current-state)
                              current-state)
              new-state-queue (loop [state-queue state-queue
                                     new-states (spin-state current-state
                                                            pop-count next-token)]
                                (if-let [new-state (first new-states)]
                                  (recur (conj state-queue new-state)
                                         (rest new-states))
                                  state-queue))]
          (recur (inc dot)
                 (om/assoc state-map current-state-key current-state)
                 new-state-queue))
        (AChart. (om/vals state-map))))))

(defn initial-chart [node]
  (AChart. [(initial-state node)]))

; Laziness knocks the memory big-O down a notch
; but doesn't get us to best-case O(n^2)--O(1) for CLR(k) grammars
; because we store matches in the chart. Push parsing could be added in the future
; to accomplish this.
;
; TODO: don't double-dip on char2
(defn run-automaton-helper [input current-chart tokenizer pos]
  (cons current-chart
        (lazy-seq
          (when-let [thechar (first input)]
            (let [rest-input (rest input)
                  char2 (if (seq rest-input) (tokenizer (first rest-input)) ::term)
                  next-chart (process-chart current-chart
                                            (tokenizer thechar) thechar char2 pos)]
              (if (seq (states next-chart))
                (run-automaton-helper (rest input) next-chart tokenizer (inc pos))
                (list next-chart))))))) ; Puts the empty chart at the end

; Runs the automaton, returning a sequence of charts
(defn run-automaton [initial-node input tokenizer]
  (run-automaton-helper input (initial-chart initial-node) tokenizer 0))
