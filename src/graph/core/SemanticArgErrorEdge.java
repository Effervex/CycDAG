package graph.core;

public class SemanticArgErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private Node proposedNode_;
	private int argNum_;
	private DAGNode predicate_;

	public SemanticArgErrorEdge(DAGNode predicate, int argNum, Node proposedNode) {
		predicate_ = predicate;
		proposedNode_ = proposedNode;
		argNum_ = argNum;
	}

	@Override
	public String getError() {
		return proposedNode_ + " is not a valid argument for arg " + argNum_
				+ " of " + predicate_ + ".";
	}

	@Override
	public Node[] getNodes() {
		return new Node[] { predicate_, PrimitiveNode.parseNode("" + argNum_),
				proposedNode_ };
	}

}
