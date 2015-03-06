package graph.core;

public class CollectionOrderErrorEdge extends DAGErrorEdge implements
		RetryableErrorEdge {
	private static final long serialVersionUID = 1L;
	private Node[] edgeNodes_;

	public CollectionOrderErrorEdge(Node[] edgeNodes) {
		edgeNodes_ = edgeNodes;
	}

	@Override
	public String getError(boolean isPretty) {
		return edgeNodes_[2].getIdentifier(isPretty)
				+ " is the wrong collection order for " + edgeNodes_[1]
				+ " using the " + edgeNodes_[0] + " relation.";
	}
	
	@Override
	public Node[] getNodes() {
		return edgeNodes_;
	}
}
