# Clearley

The better, easier way to parse, in Clojure. There's an Earley parser under the hood,
hence the name Clearley.
Clearley fits naturally in Clojure and handles all context-free grammars,
no strings attached. Just define your parse rules and go.

## Example

Here's a simple calculator written in Clearley. Parse rules and actions
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
; The below converts a char digit to a Clojure number
(def digit (token-range \0 \9
  (fn [c] (- (int c) (int \0)))))

(def my-calculator (build-parser sum))

user=> (execute my-calculator "1+1")
2
```

More examples can be found in test/clearley/examples.

## Usage

Clearley is alpha software. All the base planned functionality of a context-free
grammar parser is there. The next steps are to work on documentation,
verify edge cases, and work on internals.

Documentation coming soon.

## Why yet another parser library?

Clearley arose when a few projects I'd worked on, and planned to work on in the future,
needed a parser. Unfortunately, parsing is "the solved problem that isn't"[1].
The world of parsers is a hazardous one, full of booby traps like
left-recursion, "shift-reduce conflicts", lookahead sets, noncomposability of grammars;
and impeding you as you navigate this minefield are requirements to handle things
like tokenization, AST processing, and if you're unlucky a multi-stage build process.
This makes it difficult to use parsers in a exploratory, flexible, agile manner.
Yet the theory of context-free grammars is simple and provably tractable by computer--
so why should parsing be hard?

## Caveats

There are three major obstacles in Clearley right now:
1) The rule/defrule syntax is somewhat confusing, with some broken edge cases.
This is priority number 1.
2) Error reporting could use some work. Right now, Clearley simply fails with
"Failure to parse".
3) Clearley is slow.

Because the underlying machine--context-free grammar parsing--is very well understood,
issues 2 and 3 are solvable by applying current, well-understood techniques. This
is probably the near future of Clearley should adoption and work continue.

## References

[1] "Parsing: the solved problem that isn't". Laurence Tratt. Retrieved June 18, 2012 at
<http://tratt.net/laurie/tech_articles/articles/parsing_the_solved_problem_that_isnt>.

## License

Copyright © 2012 Mike Thvedt. All rights reserved.

Distributed under the Eclipse Public License, the same as Clojure,
which can be found in the file license.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
You must not remove this notice, or any other, from this software.
