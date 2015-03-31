package graph.inference;

/**
 * The possible query results for a query. Note that QueryResult.ALL is not
 * really a result, it's more what sort of result we want.
 *
 * @author Sam Sarjant
 */
public enum QueryResult {
	TRUE, FALSE, NIL, ALL;
}
