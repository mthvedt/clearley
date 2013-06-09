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
  #_(trees [parser input] "Parse the given input with the given parser, yielding
                        a potentially lazy seq of match trees.")
  #_(multitree [parser input] "Parse the given input with the given parser, yielding
                            a nondeterministic match tree.")
  (execute [parser input] "Parse the given input with the given parser, yielding
                          a result."))

(defprotocol ChartParser
  (print-charts [parser input] "Prints this parser's charts to *out*.
                               Only applies to chart parsers.
                               Format is not fixed. A good explanation of parse charts
                               (for an Earley parser, but same idea) is at
                               http://www.wikipedia.org/wiki/Earley_parser."))
; TODO
; * faster building
; * some notion of passthrough/hide?

(defn quentin-parser
  "Constructs a parser given a grammar and goal symbol, using the Quentin parsing
  engine."
  [goal grammar & opts]
  (let [opts (apply hash-map opts)
        mem-atom (atom {})
        q-parser-context (atom @mem-atom)
        match-parser-context (atom @mem-atom)
        myns (q/new-ns)
        goal (backtick/resolve-symbol goal)
        ; TODO remove duplication
        my-parse-fn (q/parse-fn grammar goal myns q-parser-context opts)]
    (reify
      Parser
      (execute [_ input] (q/parse my-parse-fn input myns q-parser-context opts)))))

(def parser quentin-parser)

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
  [builder goal & opts] `(build-parser-with-ns ~builder '~goal *ns* ~@opts))

(defn build-parser-with-ns
  "Build a parser in a given ns from the given goal rule."
  [builder goal thens & opts]
  (binding [*ns* thens]
    (apply builder goal (build-grammar-with-ns goal *ns*) opts)))
