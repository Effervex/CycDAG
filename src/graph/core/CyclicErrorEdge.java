package graph.core;

import org.apache.commons.lang3.StringUtils;

public class CyclicErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private Node[] cyclicAssertion_;

	public CyclicErrorEdge(Node[] assertion) {
		cyclicAssertion_ = assertion;
	}

	@Override
	public String getError() {
		return "Asserting (" + StringUtils.join(cyclicAssertion_, " ")
				+ ") would create a cycle.";
	}

	@Override
	public Node[] getNodes() {
		return cyclicAssertion_;
	}

}
