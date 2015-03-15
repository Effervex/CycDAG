package graph.core;

import org.apache.commons.lang3.StringUtils;

import graph.inference.QueryObject;
import graph.module.NLPToStringModule;

public class NegatedErrorEdge extends DAGErrorEdge implements
		RetryableErrorEdge {
	private static final long serialVersionUID = 3921728486130853014L;
	private Node[] contradictoryNodes_;

	public NegatedErrorEdge(boolean negated, Node[] assertedNodes) {
		if (negated)
			contradictoryNodes_ = new Node[] {
					CommonConcepts.NOT.getNode(CycDAG.selfRef_),
					new OntologyFunction(assertedNodes) };
		else
			contradictoryNodes_ = assertedNodes;
	}

	@Override
	public String getError(boolean isPretty) {
		String edge = null;
		if (isPretty) {
			NLPToStringModule nlpModule = (NLPToStringModule) DirectedAcyclicGraph.selfRef_
					.getModule(NLPToStringModule.class);
			if (nlpModule != null)
				edge = nlpModule.edgeToString(new QueryObject(
						contradictoryNodes_), false, false, false, false);
		} else
			edge = StringUtils.join(contradictoryNodes_, ' ');

		return "The edge '" + edge
				+ "' is contradicted by an existing assertion(s).";
	}

	@Override
	public Node[] getNodes() {
		return contradictoryNodes_;
	}

}
