# Clearley

```
[clearley "0.3.0-SNAPSHOT"]
```

Parsing for Earthlings. Parsing without compromise.

## Elevator pitch

Clearley is born of a fustration with the current state-of-the-art in parsers, which are alternately of limited power or too difficult to use. Clearley offers the following:

* *Ease of use without comprimising power.* There's a front-end API to define what your data looks like, and how to process it, in one step. You can use a Kern-style bind operation, or a Clojure core-style defrule, or you can roll your own because Clearley is also…
* *Fully programmable.* Parsers are defined in 100% Clojure fns and macros. No EBNF or regexes required. Parser rules are simple data structures. Rules can be plugged into other rules and behave as expected.
* Creates *parsers that do things*. No mucking around with parse trees. You tell Clearley how to give you the final product, not an intermediate representation. (But it can also make parse trees if you want.)
* *Handles any context-free grammar* in polynomial time. Most real-world grammars parse in linear time. You do not have to worry about whether your set of parse rules will work. All valid parse rules will work.
* Can parse *any stream of input*, not just text. Clearley is also scannerless--it doesn't need a separate tokenizer step.

## Crash course

Suppose you have a stream of data that you want to turn into a data structure.

The computer-science grad would say "use a parser", but parsers are complicated and hard to use. You have to define what your data looks like in a weird domain-specific language, then do some kind of parse tree transformations or embedded code generation.

But this is how Clearley works:

1. Describe what your data looks like, together with how to process that data, in natural Clojure.
2. There is no step 2.

Defining a Clearley rule looks like this:

```clojure
(defrule negative-number
  ([\- number] (- number))
```

The first part tells you that a negative number is the - character, followed by a number.

The second part tells you that, to process a negative number, just execute `(- number)`. The defrule wraps that in a fn and packages it with your rule.

Here is a calculator written in Clearley:

```clojure
(use 'clearley.core 'clearley.defrule)

(defrule plus-or-minus
  ([plus-or-minus \+ times-or-divide] (+ plus-or-minus times-or-divide))
  ([plus-or-minus \- times-or-divide] (- plus-or-minus times-or-divide))
  times-or-divide)
(defrule times-or-divide
  ([times-or-divide \* number] (* times-or-divide number))
  ([times-or-divide \/ number] (/ times-or-divide number))
  posnum) ; posnum comes from the Clearley core library

(def my-calculator (build-parser sum))

user=> (execute my-calculator "1+1")
2
```

Note the following:
* Forward references to rules are allowed. The grammar/parser builder will figure it out.
* Rules are just objects that can be combined. For instance, the rule "posnum" comes from the Clearley core library. [TODO]

Note that rules may refer to rules that haven't been defined yet. As long as they're defined when the parser is built, you're good.

More examples live in test/examples. For instance, there's a standards-compliant JSON parser. Since JSON uses a different set of tokens (different whitespace, control characters, escape characters) than does Java, this is a non-trivial task that shows off the power of Clearley.

## Working with rules

The quickest way to define rules is the defrule macro. To recap:

```
(defrule sum [sum \+ times] (+ sum times))
```

You can use the match macro to create a rule without defing a variable:
```
(def sum (match [sum \+ times] (+ sum times)))
```

You can use the bind and defbind macros, which work like let:
```
(defbind sum [num1 sum
              op '(:or \+ \-)
              num2 times]
  ((symbol (str op)) num1 num2)
```
This also shows how you can use _tagged lists_ as rules and subrules. Here, the rule `(:or \+ \-)` matches either the character + or the character -. This is bound to op. The rule body converts op into the symbol '+ or '-, then calls (+ num1 num2) or (- num1 num2).

In addition to these macros, Clearley has plenty more macros and fns available that create rules. See the [http://eightnotrump.github.io/clearley/codox/clearley.defrule.html](defrule codox).

### Scanning rules

Sometimes you want a fn that tells you if a token matches a rule. The fn ```clearley.core/scanner``` does this. For instance, this rule

```
(scanner #(java.lang.Character/isWhitespace %))
```

matches any Java whitespace character and returns it.

### Shorthand

At any point you can substitute a symbol for a rule. The symbol will be understood to point to another rule. You can also substitute any non-symbol, non-list Object. That will be understood to be a token.

You can use a list shorthand (prefix list) to define rules with default actions:

```
(def whitespace `(:or \space \tab \newline \return))
```

This creates an :or rule that matches any one of thsoe four characters, and returns the matched character. Prefix lists may be any (http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/sequential?)[sequential] object, which includes vectors and all seqs.

You can mix-and-match shorthand:

(def foo `(:or bar baz \x (:seq ~(star-rule 'bitcoin) satoshi)))

### Rules in detail

All rules, in the end, are maps. A rule map has these keys:

key 	| required?	| description
---|---|---
:name 	| optional	| the rule name
:tag	| require	| the rule combinator type (see below)
:value	| required	| any subrules
:action	| optional	| the parse action. if not supplied, a reasonable default is provided (see below)
:original	| supplied by the grammar builder	| if this rule was made by a grammar builder, this is the rule it was based on

Rule behavior is defined with rule tags. These are the rule tags supported by Clearley:

tag	| description	| behavior	| default action	| notes
---|---|---|---|---
:seq	| sequence	| matches all given subrules, in order	| returns a seq	| if empty, matches the empty string
:or	| choice	| matches any one of the given subrules	| identity	| if empty, always fails
:star	| zero-or-more	| matches zero or more of one given subrule	| returns a seq
:symbol	| rule reference | matches the rule the symbol refres to	| if the symbol can't be found by the grammar/parser builder, that's an error
:token	| matches one item of input using = |	returns the matched item	|
:scanner	| given a fn, matches one item of input if (fn input) is true	| returns the matched input

## Working with match trees

The goal of Clearley is to work with parse actions directly.

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

## What's under the hood

Clearley uses a nondeterministic Earley automaton. The name is a combination of 'Clojure', 'clear', and 'Earley'. The automaton is based on Aycock and Horspool's Practical Earley Parsing.

If you want, you can look at the parser's parse charts directly, but note that the underlying algorithm and its textual representation is in flux. [ TODO ]

## Disadvantages

Clearley is beta software and has a few drawbacks:

* Disambiguation is not supported. If input can be parsed in multiple ways, Clearley will silently pick one. This undesirable behavior will probably be changed in the future.
* Clearley is not (yet) highly performant. Different libraries are available if you need very fast code.
* Error reporting can use some work. Some rudimentary tools to inspect and diagnose errors can be found [ TODO ].

## Next steps

* Better ambiguity detection, error detection, and recovery.
* More performance improvements.
* Work on docs and the standard library.

## License

Clearley is licesned under the EPL, the same as Clojure.
