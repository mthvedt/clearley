package clearley;

import clojure.lang.ISeq;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mike
 * Date: 5/6/13
 * Time: 2:46 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ParseState {
	ParseState shift(Object o);
	ParseState reduce(Object o, int rval);
	ISeq input();
	List<Object> output();
	int pos();
	Object peek();  // todo obsolete
	public int getGoto();
	public ParseState setGoto(int theGoto);
	public Object returnValue(); // todo obsolete
	public void setReturnValue(Object returnValue);
	public Object getCurrent();
	public Object hasCurrent();    // returns truthy or falsey
}
