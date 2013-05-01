# Clearley

_N.B.: Version 3.0 is beta, and unstable._

```
[clearley "0.3.0-SNAPSHOT"]
```

Parsing for Earthlings.

## Overview

Clearley is born of a fustration with the current state-of-the-art in parsers, which are alternately of limited power or difficult to use (or both).

Clearley is a *general context-free grammar parser*, meaning that it can match any set of rules in polynomial time (and in practice usually linear time), and always find a correct match if one exists. Rules are just maps and objects, defined with Clojure macros, so you can manipulate, mix-and-match, and generate them as you would with any other Clojure library. Finally, Clearley is designed not just to make parse trees but to evaluate input to yield a final product.

Sitting on top is a macro library to make parsing as easy as pattern matching. After all, parsing _is_ pattern matching.

## Crash course

Parsing is for whenever you have a stream of structured input that you want to turn into output. This is what it looks like with Clearley:

```clojure
(def h (match [\h] "Hello, "))
(def w (match [\w] "world!"))
(def goal (match [h w] (str h w)))

(execute (build-parser goal) "hw")
=> "Hello, world!"
```

The rule 'h' matches the letter h, and returns "Hello, ". The letter w matches w, and returns "world!" The rule goal matches the rule h, followed by the rule w, and returns the string concatenation of the two results.

Of course there is a defmatch macro also. It works kind of like defn. There is also a shortcut where rules can point to other rules directly.

Here is a calculator written in Clearley:

```clojure
(use 'clearley.core 'clearley.match 'clearley.lib)

(defmatch sum
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term))
  term)
(defmatch term
  ([term \* natnum] (* term natnum))
  ([term \/ natnum] (/ term natnum))
  natnum)

(def my-calculator (build-parser sum))

user=> (execute my-calculator "1+1")
2
```

In particular, note the following:

* Rules are modular and can be combined. The rule 'natnum' comes from the Clearley library. It matches any positive number and returns it.
* Forward-references are handled naturally. As long as a symbol is visible when the grammar is built, you're good.
* The order of subrules--sum => sum + term--enforces left-to-right evaluation. Clearley can handle ambiguous grammars, so if the order doesn't matter, you could have said sum => sum + sum.

More examples live in test/examples. For instance, there's a standards-compliant JSON parser. Since JSON uses a different set of tokens (different whitespace, control characters, escape characters) than does Java, this is a non-trivial task.

## Working with rules

The quickest way to define rules is the defmatch macro. To recap:

```
(defmatch sum [sum \+ times] (+ sum times))
```

You can also substitute names in defmatch, by supplying a name-rule pair:
```
(defmatch sum [(foo sum) \+ (bar times)] (+ foo bar))
```

In defmatch, any symbols will _not_ be resolved. This allows them to function as forward references, so you can make recursive grammars. The parser/grammar builder will figure out the symbols at build time.

The match macro is like defmatch, but doesn't def anything:
```
(def sum (match [sum \+ term] (+ sum term)))
```

For more complicated rules, the bind and defbind macros work like Clojure's let macro:
```
(defbind sum [num1 sum
              op (or-rule [\+ \-])
              num2 term]
  ((symbol (str op)) num1 num2)
```

Here, the rule op matches either the + or - character. The rule body converts op into the symbol '+ or '-, then calls (+ num1 num2) or (- num1 num2).

In addition to these macros, Clearley has plenty more macros and fns available that create rules. I'm also working on a _standard library_; it's not complete, but generally any rule I end up using in different places I stick in the standard library. See the [http://eightnotrump.github.io/clearley/codox/clearley. defmatch.html](defmatch documentation).

### Scanning rules

Sometimes you want a fn that tells you if a token matches a rule. The fn ```clearley.core/scanner``` does this. For instance, this rule

```
(scanner #(java.lang.Character/isWhitespace %))
```

matches any Java whitespace character and returns it.

### Rules in detail

Rules are ordinary maps. This is what they look like:

key 	| required?	| description
---|---|---
:name 	| optional	| the rule name
:tag	| required	| the rule combinator type (see below)
:value	| required	| any subrules
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

### Shorthand

All rules, in the end, are maps, but Clearley supports simpler data structures when maps aren't needed. There are four kinds in total.

* Maps, described above. The grammar builder turns all rules into maps, in the end.
* Symbols. These point to other rules.
* Token literals. These are anything that aren't maps, sequences, or symbols.
* Tagged sequences.

Tagged sequences look like this:

```
(def whitespace `(:or \space \tab \newline \return))
```

This creates an :or rule that matches any one of thsoe four characters, and returns the matched character. Prefix lists may be any [sequential](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/sequential?) object--including vectors, lists, and all seqs.

You can mix-and-match shorthand:

(def foo `(:or bar baz \x (:seq ~(star-rule 'bitcoin) satoshi)))

but note that tagged sequences don't work in match/defmatch, because that's how match names subrules.

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

A grammar is a map from symbols to rules. The grammar builder will 'normalize' all rules, converting them to maps, converting symbols to qualified symbols, and looking up any symbol in the current namespace. These grammars are maps and can be manipulated like any other. If a rule is normalized, the original rule will be mapped to :original. It looks like this (truncated for brevity):

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

Clearley uses a nondeterministic Earley automaton with CLR(1) lookahead. The name is a combination of 'Clojure', 'clear', and 'Earley'. The automaton is based on Aycock and Horspool's Practical Earley Parsing. Because it uses lookahead, it can handle right-recursive grammars in linear time, unlike most Earley parsers.

If you want, you can look at the parser's parse charts directly, but note that the underlying algorithm and its textual representation is in flux. The fn for this is `print-charts`.

## Disadvantages

Clearley is beta software and has a few drawbacks:

* Disambiguation is not supported. If input can be parsed in multiple ways, Clearley will silently pick one. This undesirable behavior will probably be changed in the future.
* Clearley is not (yet) highly performant. Different libraries are available if you need very fast code.
* No error reporting, you have to debug with matches/charts.

## Next steps

* Better ambiguity detection, error detection, and recovery.
* More performance improvements.
* Work on docs and the standard library.

## License

Clearley is licesned under the EPL, the same as Clojure.
