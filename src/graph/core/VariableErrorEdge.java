package graph.core;

public class VariableErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private static VariableErrorEdge instance_ = new VariableErrorEdge();

	@Override
	public String getError() {
		return "Variable(s) found in assertion.";
	}

	@Override
	public Node[] getNodes() {
		return null;
	}

	public static VariableErrorEdge getInstance() {
		return instance_;
	}

}
