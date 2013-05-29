package clearley;

public class TransientParseState<S extends ParseStream> implements ParseState<S> {
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

	@Override
	public String toString() {
		return "TransientParseState{" +
				"theGoto=" + theGoto +
				", stream=" + stream +
				'}';
	}
}