# Clearley

Parsing for Earthlings.

## Repository

lein:
```
[clearley "0.0.2.SNAPSHOT"]
```

maven:
```
<dependency>
  <groupId>clearley</groupId>
  <artifactId>clearley</artifactId>
  <version>0.0.2.SNAPSHOT</version>
</dependency>
```

## Crash course

This is a a simple calculator written in Clearley. Parse rules and actions
are defined together in a style similar to defn.

```clojure
(use 'clearley.core)

(defrule sum
  ([sum \+ term] (+ sum term)) ; left associative
  ([sum \- term] (- sum term))
  ([term] term))
(defrule term
  ([term \* number] (* term number))
  ([term \/ number] (/ term number))
  ([number] number))
(defrule number
  ([\- number] (- number))
  ([number digit] (+ (* 10 number) digit))
  ([digit] digit))
; Convert a char digit to a Clojure number
(def digit
  (char-range \0 \9
    (fn [c] (- (int c) (int \0)))))

(def my-calculator (build-parser sum))

user=> (execute my-calculator "1+1")
2
```

More examples live in test/examples. For example, there's a fully valid JSON parser.

## Bullet points

* Clearley is Clojure-oriented. The 'defrule' macro library works in a style like that of defn. Or you can manipulate grammars and rule records directly.
* Clearley will parse any set of rules and will always parse a string when possible. Many CFG grammars (LL, *LR) have restrictions on the rules, and memoizing parsers (Packrat) may fail to find valid parses.
* Combining grammars is easy, and adding new rules will never break existing parses.
* Everything parses in polynomial time. Many real world grammars parse in O(n) time.

## Disadvantages

Clearley is alpha software and has a few drawbacks:

* Disambiguation is not supported. If input can be parsed in multiple ways, Clearley will silently pick one. This undesirable behavior will probably be changed in the future.
* Clearley is not (yet) highly performant. Different libraries are available for this purpose, such as GNU Bison.
* Error reporting is currently minimal.

The API may also change a little over time. I'm pretty pleased with it but some rough edges could use polishing.

## Defining rules

The main macro is the defrule macro, which looks a lot like defn. The difference is the arguments aren't just symbols, they refer to other rules, which may or may not be defined yet.

```clojure
(defrule term
  ([term \* number] (* term number))
  ([term \/ number] (/ term number))
  ([number] number))
```

A rule contains a sequence of clauses. If a clause is a symbol, it can be used in a defrule body. To match a rule, the parser must match all its clauses, in order.

A clause can be any of the following:

* Any instance of Rule. Rules can be embedded in anonymous rules.
* Any sequence or vector of clauses. This matches any one of those subclauses.
* Any symbol. Symbols map to sequences of rules in a grammar. If you use symbols, you probably want to use build-grammar or build-parser. When building a grammar, a symbol must be resolvable to any clause.
* Any other Object. Objects that don't match one of the above are interpreted as tokens. These Objects match a single input token using =.

In a built grammar, rules map to sequences of other rules. A non-rule clause will be wrapped up in an anonymous rule with the identity action. This doesn't affect behavior but affects match trees and parse charts.

A rule is a record that implements an internal interface (RuleKernel). I might make them pure maps in the future, but this is how it is for now. Make sure that if you manipulate rule records, you get instances of the record back. Also, the structure of the record might change to something simpler.

N.B.: The empty rule is not supported. This is usually easy to work around except you cannot parse the empty string.

## Working with match trees

You can get a match tree directly from the parser in the following way:

```clojure
user=> (parse my-calculator "1+2")
#clearley.rules.Match{:rule #clearley.rules.RuleImpl{:kernel ...}}
```

There's some crude pretty printing for matches:

```clojure
user=> (print-match (parse my-calculator "1+2"))
 sum
   sum
     term
       pow
         numexpr
           number
             clearley.rules.RuleImpl@4b2e1a6c
               '1'
   '+'
   term
     pow
       numexpr
         number
           clearley.rules.RuleImpl@4b2e1a6c
             '2'
```

You can also view parse charts:

(TODO)

## Working with grammars and rule objects

A grammar is a map from symbols to seqs of rules.

```clojure
user=> (build-grammar sum)
{digit (#clearley.rules.RuleImpl{:kernel (...) })}
user=> (pprint *1)
{digit
 ({:kernel
   {:rulefn
    #<core$char_range$fn__1939 clearley.core$char_range$fn__1939@5f36ba9b>,
    :scanned false},
   :name nil,
   :action
   #<calculator$fn__2309 clearley.examples.calculator$fn__2309@21546f3>}),
 number
 ...}
```

You can manipulate it as you'd manipulate any other map. For nondeterministic grammars, the order of rules might make a difference, but exact behavior is unspecified for now.

## Parsing in detail

(TODO)

## What's under the hood

Currently Clearley is using a GLR parser with Earley-like charts. The code is designed such that new backends can be coded and plugged in. The rule API is seperate from the parser itself.

## Next steps

* Improve the rules API. Right now they are records that implement an interface.
* Better ambiguity detection, error detection, and recovery.
* Big-O performance improvements, such as using lookahead to parse right-recursive grammars in linear time.

Please open an issue if you find any issues. If you have any suggestions, ideas, or have written some fns/libs that might be useful, please let me know!

## References

# License

Clearley is licesned under the EPL, the same as Clojure.