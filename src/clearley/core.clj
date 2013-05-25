(ns clearley.core
  "The Clearley parser and related fns."
  (require [clojure string pprint]
           [clearley.rules :as rules]
           [clearley.quentin :as q]
           [clearley.clr :as clr]
           [uncore.throw :as t]
           backtick)
  (use clearley.match clearley.grammar uncore.core uncore.memo))

(defprotocol Parser
  (execute [parser input] "Parse the given input with the given parser, yielding
                          a result."))

(defprotocol ChartParser
  (^:private charts [parser input]) ; Yields raw charts. Not for human consumption
  (print-charts [parser input] "Prints this parser's charts to *out*.
                               Format is not fixed. A good explanation of parse charts
                               (for an Earley parser, but same idea) is at
                               http://www.wikipedia.org/wiki/Earley_parser."))

#_(defn parse
  "Parse the given input with the given parser, yielding a match tree."
  [parser input]
  (q/finalize-state (parse-state parser input)))

; TODO
; * parse trees
; * faster building
; * some notion of passthrough/hide?

(defn #_quentin-parser parser
  "Constructs a parser given a grammar and goal symbol, using the Quentin parsing
  engine."
  [goal grammar & opts]
  (let [opts (apply hash-map opts)
        mem-atom (atom {})
        ; TODO split
        ; TODO this is hideous
        _ (with-memoizer mem-atom (clr/build-item-sets goal grammar))
        q-parser-context (atom @mem-atom)
        match-parser-context (atom @mem-atom)
        myns (q/new-ns)
        goal (backtick/resolve-symbol goal)
        ; TODO remove duplication
        my-parse-fn (q/parse-fn grammar goal myns q-parser-context opts)]
    (reify
      Parser
      (execute [_ input] (q/parse my-parse-fn input myns q-parser-context opts))
      #_ChartParser
      #_(charts [_ input] (parse-fn input))
      #_(print-charts [_ input] (earley/pstr-charts (parse-fn input))))))

#_(defn parser
  "Constructs a parser given a grammar and goal symbol."
  [goal grammar]
   (let [mem-atom (atom {})
         myns (q/new-ns)
         goal (backtick/resolve-symbol goal)
         my-parse-fn (q/parse-fn grammar goal myns mem-atom)]
   (reify
     Parser
     (execute [_ input] (q/parse my-parse-fn input myns mem-atom))
     #_ChartParser
     #_(charts [_ input] (parse-fn input))
     #_(print-charts [_ input] (earley/pstr-charts (parse-fn input))))))

; TODO work on this
#_(defn print-match
  "Rudimentary match-tree pretty printing to *out*."
  [match]
  ((fn f [{:keys [rule submatches]} depth]
     (println (apply str (repeat depth " ")) (get rule :name (str rule)))
     (runmap #(f % (+ depth 2)) submatches))
     match 0)
  nil)

#_(defn take-action
  "Executes the parse actions for a parser match."
  [match]
  (rules/take-action* match))

(defmacro build-parser
  "Build a parser in the current ns from the given goal rule."
  [goal & opts] `(build-parser-with-ns '~goal *ns* ~@opts))

(defn build-parser-with-ns
  "Build a parser in a given ns from the given goal rule."
  [goal thens & opts]
  (binding [*ns* thens]
    (apply parser goal (build-grammar-with-ns goal *ns*) opts)))
