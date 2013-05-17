package clearley;

import clojure.lang.ISeq;

public class TransientParseState implements ParseState {
	int pos = 0;
	int theGoto = -1;
	ISeq input;
	Object current;
	Object returnValue;

	public TransientParseState(ISeq input) {
		assert input == null || clojure.lang.RT.seq(input) != null;
		this.input = input;
		if (input != null) {
			this.current = input.first();
		}
	}

	public TransientParseState shift(Object o) {
		input = input.next();
		if (input != null) {
			current = input.first();
		}
		pos++;
		return this;
	}

	public TransientParseState reduce(Object o, int theGoto) {
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

	// TODO roll into reduce
	public void setReturnValue(Object returnValue) {
		this.returnValue = returnValue;
	}

	public Object getCurrent() {
		return current;
	}

	public Object hasCurrent() {
		return input;
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
				//", output=" + output +
				", returnValue=" + returnValue +
				'}';
	}
}