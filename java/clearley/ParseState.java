package clearley;

/**
 * Created with IntelliJ IDEA.
 * User: mike
 * Date: 5/6/13
 * Time: 2:46 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ParseState {
	ParseState shift();
	ParseState reduce(int theGoto);
	int pos();
	public int getGoto();
	public ParseState setGoto(int theGoto);
//	public Object returnValue();
	public long currentInput();
//	public Object hasInput();    // returns truthy or falsey
	public boolean hasInput();
}
