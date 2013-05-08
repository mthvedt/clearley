package clearley;

import clojure.lang.ISeq;

import java.util.ArrayList;
import java.util.List;

public class TransientParseState implements ParseState {
	public int pos = 0;
	public int theGoto = -1;
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