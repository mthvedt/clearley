package clearley;

/**
 * Created with IntelliJ IDEA.
 * User: mike
 * Date: 5/24/13
 * Time: 10:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class StringParseStream implements CharParseStream {
	final String input;
	int pos;
	long current;

	public StringParseStream(String input) {
		this.input = input;
		this.pos = -1;
		shift();
	}

	public void shift() {
		pos++;
		if (pos < input.length())
		current = input.charAt(pos);
	}

	public long pos() {
		return pos;
	}

	public boolean hasInput() {
		return pos < input.length();
	}

	public long current() {
		return current;
	}

	@Override
	public String toString() {
		return "StringParseStream{" +
				"input='" + input + '\'' +
				", pos=" + pos +
				", current=" + current +
				'}';
	}
}
