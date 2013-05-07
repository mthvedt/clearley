package clearley;

import clojure.lang.ISeq;

import java.util.ArrayList;
import java.util.List;

public class TransientParseState implements ParseState {
	public int pos = 0;
	public int lastReturnId = -1;   // todo rename
	public ISeq input;
	public ArrayList<Object> output = new ArrayList<Object>();

	public TransientParseState(ISeq input) {
		this.input = input;
	}

	public TransientParseState shift(Object o) {
		input = input.next();
		pos++;
		output.add(o);
		return this;
	}

	public TransientParseState reduce(Object o, int lastReturnId) {
		output.add(o);
		this.lastReturnId = lastReturnId;
		return this;
	}

	public Object peek() {
		return output.get(output.size() - 1);
	}

	public int lastReturnId() {
		return lastReturnId;
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