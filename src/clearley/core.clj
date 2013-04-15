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
  (use [clearley defrule]
       uncore.core))

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
  "Constructs a parser given a map of rules,
  a goal clause, and an optional tokenizer."
  ([goal rules]
   (parser goal identity rules))
  ([goal tokenizer rules]
   (reify
     Parser
     (parse [_ input]
       ; For now, only return first match. If failure, last chart will be empty
       (-> (glr/parse-charts input rules tokenizer goal) last glr/scan-goal first))
     ChartParser
     (charts [_ input]
       (glr/parse-charts input rules tokenizer goal))
     (print-charts [_ input]
       (glr/pstr-charts (glr/parse-charts input rules tokenizer goal))))))

(defn print-match
  "Pretty-prints a match tree to *out*."
  [match]
  ((fn f [{:keys [rule submatches]} depth]
     (println (apply str (repeat depth " ")) (pr-str rule))
     (domap #(f % (+ depth 2)) submatches))
     match 0)
  nil) ; don't return a tree full of nils

(defn take-action
  "Executes the parse actions for a parser match."
  [match]
  (if (nil? match)
    (throw (RuntimeException. "Failure to parse"))
    (let [{:keys [rule submatches]} match
          subactions (map take-action submatches)
          action (rules/action rule)]
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (rules/rule-str rule) ", "
                                         "was given " (count subactions))
                                    e)))))))

(defn execute
  "Parses some input and executes the parse actions."
  [parser input]
  (take-action (parse parser input)))

(defmacro build-parser
  "Build a parser in the current ns from the given goal rule and an
  optional tokenizer."
  ([goal]
   `(build-parser ~goal identity))
  ([goal tokenizer]
   `(build-parser-with-ns '~goal ~tokenizer *ns*)))

(defn build-parser-with-ns
  "Build a parser in a given ns from the given goal rule and tokenizer."
  [goal tokenizer thens]
  (parser goal tokenizer (build-grammar-with-ns goal thens)))
