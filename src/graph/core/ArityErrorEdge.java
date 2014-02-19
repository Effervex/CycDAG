package graph.core;

public class ArityErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private int numArgs_;
	private DAGNode predicate_;

	public ArityErrorEdge(DAGNode predicateNode, int numArgs) {
		predicate_ = predicateNode;
		numArgs_ = numArgs;
	}

	@Override
	public String getError() {
		return "Predicate " + predicate_ + " should only have " + numArgs_
				+ " arguments.";
	}

	@Override
	public Node[] getNodes() {
		return new Node[] { predicate_ };
	}
}
