package graph.core;

/**
 * A class for accessing the inner nodes of a modified edge or query more
 * easily.
 *
 * @author Sam Sarjant
 */
public class EdgeModifier {
	/**
	 * Returns the encapsulated nodes in an array of nodes if they are modified.
	 *
	 * @param nodes
	 *            The possibly encapsulated nodes to return (without modifier).
	 * @param dag
	 *            The DAG access.
	 * @return Either the same nodes if no modifier, or the nodes in the 2nd
	 *         argument position of the modified node array..
	 */
	public static Node[] getUnmodNodes(Node[] nodes, DirectedAcyclicGraph dag) {
		if (nodes.length == 2 && nodes[1] instanceof OntologyFunction
				&& isModifier(nodes[0], dag))
			return getUnmodNodes(((OntologyFunction) nodes[1]).getNodes(), dag);
		return nodes;
	}

	/**
	 * If the given node is a modifier node.
	 *
	 * @param node
	 *            The node to check.
	 * @param dag
	 *            The DAG access.
	 * @return True if it is a modifier node (NOT or REMOVED)
	 */
	private static boolean isModifier(Node node, DirectedAcyclicGraph dag) {
		if (isNegated(node, dag))
			return true;
		if (isRemoved(node, dag))
			return true;
		return false;
	}

	/**
	 * Returns the unmodified nodes of an edge.
	 *
	 * @param edge
	 *            The edge to extract nodes for.
	 * @param dag
	 *            The DAG access.
	 * @return The nodes of the edge without modifiers.
	 */
	public static Node[] getUnmodNodes(Edge edge, DirectedAcyclicGraph dag) {
		return getUnmodNodes(edge.getNodes(), dag);
	}

	public static boolean isNegated(Node node, DirectedAcyclicGraph dag) {
		return node.equals(CommonConcepts.NOT.getNode(dag));
	}

	public static boolean isNegated(Edge edge, DirectedAcyclicGraph dag) {
		return isNegated(edge.getNodes()[0], dag);
	}

	public static boolean isRemoved(Node node, DirectedAcyclicGraph dag) {
		return node.equals(CommonConcepts.REMOVED.getNode(dag));
	}

	public static boolean isRemoved(Edge edge, DirectedAcyclicGraph dag) {
		return isRemoved(edge.getNodes()[0], dag);
	}

	public static boolean isSpecial(Edge edge, DirectedAcyclicGraph dag) {
		if (isNegated(edge, dag))
			return true;
		if (isRemoved(edge, dag))
			return true;
		return false;
	}

	public static boolean isSpecial(Node[] edgeNodes, DirectedAcyclicGraph dag) {
		if (isNegated(edgeNodes[0], dag))
			return true;
		if (isRemoved(edgeNodes[0], dag))
			return true;
		return false;
	}
}
