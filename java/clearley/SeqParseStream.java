package clearley;

import clojure.lang.ISeq;

/**
 * Created with IntelliJ IDEA.
 * User: mike
 * Date: 5/24/13
 * Time: 10:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class SeqParseStream implements ObjParseStream {
	ISeq input;
	long pos = 0;
	Object current = null;

	public SeqParseStream(ISeq input) {
		assert input == null || clojure.lang.RT.seq(input) != null;
		this.input = input;
		if (input != null) {
			this.current = input.first();
		}
	}

	public void shift() {
		input = input.next();
		if (input != null) {
			this.current = input.first();
		}
		pos++;
	}

	public long pos() {
		return pos;
	}

	public boolean hasInput() {
		return input != null;
	}

	public Object current() {
		return current;
	}

	@Override
	public String toString() {
		return "SeqParseStream{" +
				"input=" + input +
				", pos=" + pos +
				", current=" + current +
				'}';
	}
}
