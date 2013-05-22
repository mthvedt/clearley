package clearley;

public class TransientParseState<S extends ParseStream> implements ParseState<S> {
	int pos = 0;
	int theGoto = -1;
	S stream;

	public TransientParseState(S stream) {
		this.stream = stream;
	}

	public S stream() {
		return stream;
	}

	public int getGoto() {
		return theGoto;
	}

	public void setGoto(int theGoto) {
		this.theGoto = theGoto;
	}

	public int pos() {
		return pos;
	}

	@Override
	public String toString() {
		return "TransientParseState{" +
				"pos=" + pos +
				", theGoto=" + theGoto +
				", stream=" + stream +
				'}';
	}
}