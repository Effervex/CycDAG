package graph.core;

public class NonDAGNodeErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private Node[] edgeNodes_;
	
	public NonDAGNodeErrorEdge(Node[] edgeNodes) {
		edgeNodes_ = edgeNodes;
	}

	@Override
	public String getError() {
		return "Edge cannot have non-DAG node as first argument!";
	}

	@Override
	public Node[] getNodes() {
		return edgeNodes_;
	}

}
