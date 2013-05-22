package clearley;

/**
 * A parse stream that returns chars, except it actually returns Longs because Clojure doesn't support
 * fast primitive char casts!
 */
public interface CharParseStream extends ParseStream {
	long current();
}
