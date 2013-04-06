# TODO

Alpha:

* Clean up rule records?

Beta: 

* Clean up defrule; add direct symbol references and binding forms.
* Better error reporting for user parsers.
* Grammar composition.
* Clean up the software internals.
* Work on defrule syntax. Eventually I want to make Clearley self-hosting--
use a parser to define defrule, instead of about 90 lines of macros.
Would make a convincing POC for the 'parse any input' thesis!

More:

* Redo NPDA.
* EarleyItem protocol and polymorphism.
Leverage the JVM for max speed and flexibility.
* Expose EarleyItem protocol. Allow extension of the rule mechanism.
Infinitely-generated context-free grammars in O(n^3) time. Noam Chomsky can eat it!
The EarleyItem protocol must be defined with building a LR-NDFA in mind, leading to...
* Parser NDFA. This will be a huge performance win. See 'Practical Earley Parsing',
Aycock & Horspool 2002, to see the performance compares favorably to standard automata.
* Also test reliable disambiguity (LR vs LL in particular) when the above is implemented.
