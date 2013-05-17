package clearley;

public class TransientParseState implements ParseState {
	int pos = 0;
	int theGoto = -1;
	//ISeq input;
	String input;
	Object current;
	Object returnValue; // todo rename

	//public TransientParseState(ISeq input) {
	public TransientParseState(String input) {
		assert input == null || clojure.lang.RT.seq(input) != null;
		this.input = input;
//		if (input != null) {
//			this.current = input.first();
//		}
	}

	public TransientParseState shift(Object o) {
//		input = input.next();
//		if (input != null) {
//			current = input.first();
//		}
		returnValue = o;
		pos++;
		return this;
	}

	public TransientParseState reduce(int theGoto, Object returnValue) {
		this.returnValue = returnValue;
		this.theGoto = theGoto;
		return this;
	}

	public int getGoto() {
		return theGoto;
	}

	public TransientParseState setGoto(int theGoto) {
		this.theGoto = theGoto;
		return this;
	}

	public Object returnValue() {
		return returnValue;
	}

	public long getCurrent() {
		if (hasCurrent())
		return input.charAt(pos);
		else return '\0';   // TODO: current parser gets current and throws away input. fix this
	}

	public boolean hasCurrent() {
		return pos < input.length();
	}

	public int pos() {
		return pos;
	}

	@Override
	public String toString() {
		return "TransientParseState{" +
				"pos=" + pos +
				", theGoto=" + theGoto +
				//", input=" + input +
				", current=" + current +
				", returnValue=" + returnValue +
				'}';
	}
}