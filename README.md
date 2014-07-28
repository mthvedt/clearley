# Clearley

[![Clojars Project](http://clojars.org/clearley/latest-version.svg)](http://clojars.org/clearley)

Parsing for Earthlings.

## Overview

Writing parsers should be as easy as writing ordinary Clojure code. Or maybe slightly harder.

Clearley is a Clojure-flavored parser generator.
Parse rules are ordinary maps, and behavior can be defined with a defn-like pattern matching syntax.
Unlike many parsers, Clearley does not require the user to understand parser theory;
it accepts any valid set of parse rules and parsers all valid input in polynomial time (usually linear time).
Clearley can also handle any seq of objects, not just text.

## Crash course

Parsing is for whenever you have a stream of structured input that you want to turn into output. This is what it looks like with Clearley:

```clojure
(def h (match [\h] "Hello, "))
(def w (match [\w] "world!"))
(def goal (match [h w] (str h w)))

(execute (build-parser goal) "hw")
=> "Hello, world!"
```

The rule `h` matches the letter 'h', and returns `"Hello, "`. The rule `w` matches 'w', and returns `"world!"`
The rule `goal` matches the rules `h` followed by `w`, and concatenates the results.

Clearley will find any possible match, no matter how much backtracking or ambiguity are in the rulesets.

Of course there's also a `defmatch` macro. Here is a calculator written in Clearley using `defmatch`:

```clojure
(use 'clearley.core 'clearley.match 'clearley.lib)

(defmatch sum
  ; Defn-like syntax. You define the pattern match,
  ; and can refer to matched rules in the body.
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term))
  ; Rules can also point directly to other rules.
  term)
(defmatch term
  ; The rule 'natnum' comes from the namespace clearley.lib.
  ([term \* natnum] (* term natnum))
  ([term \/ natnum] (/ term natnum))
  natnum)

(def my-calculator (build-parser sum))

user=> (execute my-calculator "1+1")
2
```

The above demonstrates the following:

* Rules are modular and can be combined. The rule `natnum` comes from the namespace `clearley.lib`. It matches any positive number and returns it.
* Forward-references require no special handling. The parser builder resolves all symbols at build time.
* The order of subrules--sum => sum + term--enforces left-to-right evaluation.
Clearley can handle ambiguous grammars, so if the order doesn't matter, you could have said sum => sum + sum.
* Clearley does not need a "tokenization" step, unlike many other parsers.

More examples live in [test/clearley/test/examples](https://github.com/eightnotrump/clearley/tree/master/test/clearley/examples).
For instance, there's a standards-compliant JSON parser. JSON uses different tokens (viz. different whitespace, control characters, and escape characters) than does Java.
This is where Clearley offers an advantage, since Clearley's design makes it natural to use alternate definitions for whitespace and so on.
_(N.B.: At least two fairly popular parsing libraries use JSON as an example, but get it wrong and miss these details.)_

Parser related stuff lives in `clearley.core` and the defmatch frontend lives in `clearley.match`.
All libraries are documented at http://eightnotrump.github.io/clearley/codox/ .

## Working with rules

The quickest way to define rules is the defmatch macro. To recap:

```clojure
(defmatch sum [sum \+ times] (+ sum times))
```

You can use `(symbol, rule)` pairs to supply alternate bindings.
```clojure
(defmatch sum [(foo sum) \+ (bar times)] (+ foo bar))
```

Defmatch allows you to refer to rules that are not yet defined.
The parser/grammar builder will look up the symbols at build time.
Defmatch will also look up symbols if it can, so you can pull in rules from different namespaces.
(This also means that you can extend rules by substituting rules into grammar maps--see below.)

The match macro is like defmatch, but doesn't def anything:
```clojure
(def sum (match [sum \+ term] (+ sum term)))
```

For more complicated rules, the bind and defbind macros can define rules with syntax like Clojure's `let`:
```clojure
(defbind sum [num1 sum
              op (or-rule [\+ \-])
              num2 term]
  ((symbol (str op)) num1 num2)
```

All of these 'bindings' are matched in sequence.
Here, the rule `op` matches either the `+` or `-` character. The rule body converts op into the symbol `'+` or `'-`, then calls `(+ num1 num2)` or `(- num1 num2)`.

Clearley also offers a full library of parsing macros and fns.
The namespace `clearely.match` contains a core library of macros and fns for defining rules.
Many more useful rules and fns for common tasks live in `clearley.lib`,
Here's the codox: [match](http://eightnotrump.github.io/clearley/codox/clearley.match.html) and [lib](http://eightnotrump.github.io/clearley/codox/clearley.lib.html).

### Scanning rules

Sometimes you want a fn that tells you if a token matches a rule. The fn `clearley.core/scanner` does this. For instance, this rule

```clojure
(scanner #(java.lang.Character/isWhitespace %))
```

matches any Java whitespace character and returns it.

### Shorthand

In addition to using the fns above, you can plug in _shorthand rules_ to other rules.
You've already seen two kinds of shorthand: symbols, which point to other rules; and plain old objects.

There's also _tagged sequences._ They look like this:

```clojure
(def whitespace `(:or \space \tab \newline \return))
```

This creates an :or rule that matches any one of those four characters, and returns the matched character.
Tagged sequences may be any [sequential](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/sequential?) object--including vectors, lists, and all seqs.

You can mix-and-match shorthand:

```clojure
(def foo `(:or bar baz \x (:seq ~(star-rule 'bitcoin) satoshi)))
```

Tagged sequences cannot be used directly in `match` or `defmatch` macros, because they clash with existing syntax.
You can refer to them indirectly by `def`ing one to a symbol.

### Low-level rules

Clearley exposes its own internal rules API, because good libraries are functional, consisten and modular.
Opaque "magic" APIs are for losers.

The `defmatch` API is just a layer on top of the low-level rules. You could, if you wanted, write your own code to make Clearley rules.

Rules can be one of four things:

* Maps, described below. The grammar builder turns all rules into maps in the end.
* Symbols. These point to other rules.
* Token literals. These are anything that aren't maps, sequences, or symbols.
* Tagged sequences.

Maps are the most general kind. They have the following keys:

key 	| required?	| description
---|---|---
:name 	| optional	| the rule name
:tag	| required	| the rule type (see below)
:value	| required	| a sequence of subrules (vector, list, seq, or any other Sequential)
:action	| optional	| the parse action. if not supplied, a reasonable default is provided (see below)
:original	| supplied by the grammar builder	| if this rule was made by a grammar builder, this is the rule it was based on

Rule behavior is defined with rule tags. These are the rule tags supported by Clearley:

tag	| description	| behavior	| default action	| notes
---|---|---|---|---
:seq	| sequence	| matches all given subrules, in order	| returns a seq	| if empty, matches the empty string
:or	| choice	| matches any one of the given subrules	| identity	| if empty, always fails
:star	| zero-or-more	| matches zero or more of one given subrule	| returns a seq
:symbol	| rule reference | matches the rule the symbol refres to	| the action of the subrule | if the symbol can't be found by the grammar/parser builder, that's an error
:token	| matches one item of input using = |	returns the matched item	| none
:scanner	| given a fn, matches one item of input if (fn input) is true	| returns the matched input | none

## Working with match trees

The goal of Clearley is to work with parse actions directly, but if you want, you can get a match tree directly from the parser in the following way (truncated for brevity):

```clojure
user=> (clojure.pprint/pprint (parse my-calculator "1+2"))
{:name :clearley.rules/goal,
  :tag :seq,
  :value
  [{:name "sum",
    :tag :symbol,
    :value [user/sum],
    :action #<core$identity clojure.core$identity@6adaf50c>,
    :original user/sum}],
  :action #<core$identity clojure.core$identity@6adaf50c>},
 :submatches
 [{:rule
   {:name sum
...}}
```

I hope to introduce better, more readable match pretty-printing in the future.

## Working with grammars and rule objects

There's a grammar builder in `clearley.grammar`. For details, check out the [docs](http://eightnotrump.github.io/clearley/codox/clearley.grammar.html).

A grammar is a map from symbols to rules. The grammar builder will 'normalize' all rules, converting them to maps, converting symbols to qualified symbols, and looking up any symbol in the current namespace. These grammars are maps and can be manipulated like any other.
If a rule is normalized, the original rule will be mapped to `:original`. It looks like this (truncated for brevity):

```clojure
user=> (def g (clearley.grammar/build-grammar sum))
#'user/g
user=> (clojure.pprint/pprint g)
{clearley.lib/natnum
 {:original
  {:name nil ... },
  :name "natnum",
  :tag :star,
  :value [ ... ]},
 user/term { ... },
 user/sum { ... },
...}
```

## What's under the hood

Clearley uses a nondeterministic Earley automaton with CLR(1) lookahead. The name is a combination of 'Clojure', 'clear', and 'Earley'. The automaton is based on [Aycock and Horspool's Practical Earley Parsing](http://courses.engr.illinois.edu/cs421/sp2012/project/PracticalEarleyParsing.pdf). Because it uses lookahead, it can handle right-recursive grammars in linear time, unlike most Earley parsers. Unfortunately the parser is somewhat slow, but there's a lot of room for performance improvements.

If you want, you can look at the parser's parse charts directly, but note that the underlying algorithm and its textual representation is in flux. The fn for this is `print-charts`.

## Disadvantages

Clearley is still in development has a few drawbacks:

* Disambiguation is not supported. If input can be parsed in multiple ways, Clearley will silently pick one. This undesirable behavior will probably be changed in the future.
* Clearley is not (yet) highly performant. Different libraries are available if you need very fast code.
* No error reporting, you have to debug with matches/charts.

## Next steps

* Better ambiguity detection, error detection, and recovery.
* More performance improvements.

## License

Clearley is licensed under the EPL, the same as Clojure.
