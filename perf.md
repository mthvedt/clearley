PERFORMANCE TIPS

* Use text parsing mode if you can. Clearley is many times faster parsing primitives than parsing objects.

* Left recursion is fast. In an LR-type parser, it's basically a loop. Right recursion requires actual recursion, and having a big stack is much slower than having a small stack on top of a loop.

* If you're using object parsing mode and you want to use tokens, make sure your input tokens have fast hash codes. 

* In text parsing mode, type hint your scanners with ^long. Because of an oversight in Clojure, type hints only work if they're a defn--so use defn and pass in a symbol, or better yet, use defscanner. (Fair warning: if you don't do the defn/symbol trick, type hints might actually be *slower*. Clojure uses *non-inlined type coercions* as a compatibility layer on top of type hinted fns, and type coercions are complex enough that the JVM won't inline them also. This is slow.)

* Avoid type hints in parse actions. Object casts are fast. Clojure's inlinable math operators are fast. Coercions are slow (see above). Quentin stores intermediate values as Objects in the current build.

* Don't worry about the size of your rule sets. Quentin is smart enough to inline "placeholder rules" that only have a singleton subrule. If the rule has the default action symbol ('clearley.grammar/fast-identity) it gets optimized to nothing.