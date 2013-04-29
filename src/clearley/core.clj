(ns clearley.core
  "Tools for parsing and processing linear input.
  The central abstraction is the context-free grammar, where one match rule
  maps to arbitrary sequences of sub-rules.
  Emphasis is on completeness, modularity, and ease of use.
  A functional API and a macro DSL are provided.
 
  See the high-level docs for a further background and overview." 
  (require [clojure string pprint]
           [clearley.rules :as rules]
           [clearley.glr :as glr]
           [uncore.throw :as t])
  (use [clearley defrule grammar] uncore.core))
; TODO integreate defrule

(defprotocol Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a match tree."))

(defprotocol ChartParser
  (charts [parser input]) ; Yields raw charts. Not for human consumption
  (print-charts [parser input] "Prints this parser's charts to *out*.
                               Format is not fixed. A good explanation of parse charts
                               (for an Earley parser, but same idea) is at
                               http://www.wikipedia.org/wiki/Earley_parser."))

(defn parser
  "Constructs a parser given a grammar and goal clause."
  ;"Constructs a parser given a grammar, a goal clause, and an optional tokenizer."
  ([goal grammar]
   ;(parser goal identity grammar))
  ;([goal tokenizer grammar]
   (let [mem-atom (atom {})
         mem-atom-2 (atom {})
         parse-fn #(glr/parse-charts % grammar identity
                                     #_tokenizer goal mem-atom mem-atom-2)]
   (reify
     Parser
     (parse [_ input]
       ; For now, only return first match. If failure, last chart will be empty
       (-> (parse-fn input) last glr/scan-goal first))
     ChartParser
     (charts [_ input] (parse-fn input))
     (print-charts [_ input] (glr/pstr-charts (parse-fn input)))))))

; TODO work on this
(defn print-match
  "Rudimentary match-tree pretty printing to *out*."
  [match]
  ((fn f [{:keys [rule submatches]} depth]
     (println (apply str (repeat depth " ")) (rules/rule-str rule))
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
  "Build a parser in the current ns from the given goal rule
  and an optional tokenizer."
  ;and an optional tokenizer."
  ([goal]
   ;`(build-parser ~goal identity))
  ;([goal tokenizer]
   `(build-parser-with-ns '~goal #_~tokenizer *ns*)))

(defn build-parser-with-ns
  "Build a parser in a given ns from the given goal rule."
  ;and tokenizer."
  [goal #_tokenizer thens]
  (parser goal #_tokenizer (build-grammar-with-ns goal thens)))
