package clearley;

/**
 * Created with IntelliJ IDEA.
 * User: mike
 * Date: 5/24/13
 * Time: 10:21 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ParseStream {
	void shift();
	long pos();
	public boolean hasInput();    // returns truthy or falsey
}
