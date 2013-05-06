package clearley;

import clojure.lang.ISeq;

import java.util.ArrayList;
import java.util.List;

public class TransientParseState implements ParseState {
	public int pos;
	public int rval;
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

	public TransientParseState reduce(Object o, int rval) {
		output.add(o);
		this.rval = rval;
		return this;
	}

	public Object peek() {
		return output.get(output.size() - 1);
	}

	public int rval() {
		return rval;
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