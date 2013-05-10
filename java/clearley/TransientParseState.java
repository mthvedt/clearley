package clearley;

import clojure.lang.ISeq;

import java.util.ArrayList;
import java.util.List;

public class TransientParseState implements ParseState {
	int pos = 0;
	int theGoto = -1;
	ISeq input;
	Object current;
	ArrayList<Object> output = new ArrayList<Object>();  // todo obsolete
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
		output.add(o);
		return this;
	}

	public TransientParseState reduce(Object o, int theGoto) {
		output.add(o);
		this.theGoto = theGoto;
		return this;
	}

	public Object peek() {
		return output.get(output.size() - 1);
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

	public void setReturnValue(Object returnValue) {
		this.returnValue = returnValue;
	}

	public Object getCurrent() {
		return current;
	}

	public Object hasCurrent() {
		return input;
	}

	public ISeq input() {
		return input;
	}

	public List<Object> output() {
		return output;
	}

	public int pos() {
		return pos;
	}
}