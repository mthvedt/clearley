package clearley;

/**
 * A parse stream that returns chars, except it actually returns ints because Clojure doesn't support
 * fast primitive char casts!
 */
public interface CharParseStream extends ParseStream {
	int current();
}
