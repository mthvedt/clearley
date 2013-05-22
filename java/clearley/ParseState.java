package clearley;

/**
 * Created with IntelliJ IDEA.
 * User: mike
 * Date: 5/6/13
 * Time: 2:46 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ParseState<S extends ParseStream> {
	S stream();
	public int getGoto();
	public void setGoto(int theGoto);
}
