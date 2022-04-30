package org.kissweb.database;

import java.util.ArrayList;

/**
 *
 *  In Java, there is no real way to detect the difference between the empty array lists
 *  <code>ArrayList&lt;Integer&gt;</code> and <code>ArrayList&lt;String&gt;</code>.
 *  This class converts a generic class into a regular class, so it can be detected.
 *  It is designed to be used in SQL with a prepared statement when you want to pass
 *  an array like in <code>select * from mytable where col1 = ANY(?)</code>
 *  <br><br>
 *  Note that <code>select * from mytable where col1 in (?)</code> must be re-written like
 *  <code>select * from mytable where col1 = ANY(?)</code>
 *  <br><br>
 *  In these cases, subclasses of this abstract class must be used rather than the generic.
 *
 * @see ArrayListInteger
 * @see ArrayListLong
 * @see ArrayListShort
 * @see ArrayListString
 */
public abstract class ArrayListType<T> extends ArrayList<T> {
}
