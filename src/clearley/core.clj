(ns clearley.core
  "The Clearley parser and related fns."
  (require [clojure string pprint]
           [clearley.rules :as rules]
           [clearley.earley :as earley]
           [clearley.quentin :as q]
           [uncore.throw :as t]
           backtick)
  (use clearley.match clearley.grammar uncore.core))

(defprotocol Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a match tree."))

(defprotocol ChartParser
  (^:private charts [parser input]) ; Yields raw charts. Not for human consumption
  (print-charts [parser input] "Prints this parser's charts to *out*.
                               Format is not fixed. A good explanation of parse charts
                               (for an Earley parser, but same idea) is at
                               http://www.wikipedia.org/wiki/Earley_parser."))

(defn parser
  "Constructs a parser given a grammar and goal symbol."
  ([goal grammar]
   (let [mem-atom (atom {})
         myns (q/new-ns)
         goal (backtick/resolve-symbol goal)]
   (reify
     Parser
     (parse [_ input]
       ; For now, only return first match. If failure, last chart will be empty
       (q/finalize-state (q/parse grammar goal input myns mem-atom)))
     #_ChartParser
     #_(charts [_ input] (parse-fn input))
     #_(print-charts [_ input] (earley/pstr-charts (parse-fn input)))))))

; TODO work on this
(defn print-match
  "Rudimentary match-tree pretty printing to *out*."
  [match]
  ((fn f [{:keys [rule submatches]} depth]
     (println (apply str (repeat depth " ")) (get rule :name (str rule)))
     (runmap #(f % (+ depth 2)) submatches))
     match 0)
  nil)

(defn take-action
  "Executes the parse actions for a parser match."
  [match]
  (rules/take-action* match))

(defn execute
  "Parses some input and executes the parse actions."
  [parser input]
  (rules/take-action* (parse parser input)))

(defmacro build-parser
  "Build a parser in the current ns from the given goal rule."
  [goal] `(build-parser-with-ns '~goal *ns*))

(defn build-parser-with-ns
  "Build a parser in a given ns from the given goal rule."
  [goal thens] (parser goal (build-grammar-with-ns goal thens)))
