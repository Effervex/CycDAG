package graph.core;

import org.apache.commons.lang3.StringUtils;

import graph.inference.QueryObject;
import graph.module.NLPToStringModule;

public class NegatedErrorEdge extends DAGErrorEdge implements
		RetryableErrorEdge {
	private static final long serialVersionUID = 3921728486130853014L;
	private Node[] contradictoryNodes_;
	private Node[] contradictionEdge_;

	public NegatedErrorEdge(boolean negated, Node[] assertedNodes,
			Node[] contradictionEdge) {
		if (negated)
			contradictoryNodes_ = new Node[] {
					CommonConcepts.NOT.getNode(CycDAG.selfRef_),
					new OntologyFunction(assertedNodes) };
		else
			contradictoryNodes_ = assertedNodes;
		contradictionEdge_ = contradictionEdge;
	}

	@Override
	public String getError(boolean isPretty) {
		if (isPretty) {
			NLPToStringModule nlpModule = (NLPToStringModule) DirectedAcyclicGraph.selfRef_
					.getModule(NLPToStringModule.class);
			if (nlpModule != null)
				return nlpModule.edgeToString(new QueryObject(
						contradictoryNodes_), false, false, false, false);
		}
		return "The edge " + StringUtils.join(contradictoryNodes_, ' ')
				+ " is contradicted by the existing assertion "
				+ StringUtils.join(contradictionEdge_, ' ') + ".";
	}

	@Override
	public Node[] getNodes() {
		return contradictoryNodes_;
	}

}
