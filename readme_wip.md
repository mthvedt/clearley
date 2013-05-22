Quentin is an __extremely fast__ bottom-up parser with an __intuitive frontend__ and a __mad-scientist__ backend. Quentin uses magic (Clojure magic) to generate code for very fast parsers on the fly.

> Quentin quote

At the center of Quentin is a hideous monster called The Beast. The Beast is a bottom-up LR automaton with a large number of soul-darkening optimizations, some more evil than others:
* Instead of using an LR(1) automaton, The Beast uses the Earley automaton described in [[Practical Earley Parsing]], avoiding the 'ambiguous return' of the classical LR(1) algorithm.
* Lane-tracing is used to shrink code-size.
* One major limitation of eval is that you can't refer to anything outside the current namespace. The Beast creates a _new namespace_ and _unmaps it_ when it's done, leaving orphaned, dynamically generated fns that all point to each other that do the parsing.

> The Beast

The Beast is a code generating parser.