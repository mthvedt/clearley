# Clearley

A parser library in Clojure with an emphasis on dynamic programming, extensibility,
ease of use, and handling all cases. Clearley wants to change parsing from
something exotic to a tool anyone can use.

## Clearley is

## Clearley isn't

## Crash course

Clearley is a context-free grammar parser. The goal of Clearley is to be
a general way to process any stream of incoming data. Clearley will accept
any context-free grammar, and most real-world use cases run in O(n) time.

Here is Clearley in action, defining a simple calculator parser.

Here's the calculator in action.

You can see more examples in the `test/examples` folder. There's a JSON parser there,
for instance.

## Defrule

Clearley provides a macro library for writing DSLs in Clojure

The main construct is defrule, which takes a rule name, a rule body,
and an action body. You can grab a parse tree if you want, but Clearley is designed
also to make it easy to process the parse tree. Here's how you grab a parse tree:

Each rule is associated with a parse action, which is a function. When a rule
is parsed successfully, Clearley can call a parse action. The parse action takes
as its arguments, in order, the results of the parse actions of the child rules.
In the above calculator example, the parse actions are pretty simple;
the construct `todo` tells Clearley whenever we encounter something of the form
`todo`, we call `(+ )`. Parse actions can be as simple or complex as you wish.
This is deferred until after parse time, so you can also think of it as crawling
the parse tree, executing parse actions recrusively from the top down.

Keywords can be used as rule names, and they map naturally to action bodies.

qualified keywords also (they become dequalified in action bodies in defrule):

Defrule lets you bind multiple rules to one clause:

inline rules:

and inline multiple rules:

## Functional API

Macros are a convenience layer for functions, not a substitute for them.
Everything in Clearley can be done with functions, which are exposed to the user.
These are described in the docs. However, beware: Clearley is still in dev
and the functional API may change.

## Wish list

Potential ideas or features for Clearley:

* Ambiguity support and disambiguation rules. Currently Clearley outputs a single
parse and pretends it's canonical.

* ClojureScript compatiblity.

* Java API.

* Error recovery when parsing.

* Infinitely-generated context-free grammars. With this we can efficiently parse
a superset of CFGs in O(3) time. The framework for this is already in place--
one would allow rules' 'predict' steps to vary based on the parse state and
matched output.

* Boolean grammars.

* A parsing automaton for better performance.
Aycock & Horspool's paper, Practical Earley Parsing, describes
how to accomplish efficient Earley parsing. It's also known that Earley parsing
is not far removed from nondeterministic LR parsing. Aycock and Horspool
also mention that good performance is available when lazily generating parsing
automata, yielding the ability to create infinite-state machines for the
infinitely-generated context-free grammars above. The branch earley-glr contains
some first steps towards a parsing automaton.

If someone writes a paper on infinite-state parsing automata, remember you heard
it here first.

## References

TODO: add references.

## License

Copyright © 2012 Mike Thvedt. All rights reserved.

Distributed under the Eclipse Public License, the same as Clojure,
which can be found in the file license.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
You must not remove this notice, or any other, from this software.
