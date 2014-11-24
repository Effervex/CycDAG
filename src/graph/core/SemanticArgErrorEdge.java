/*******************************************************************************
 * Copyright (c) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.core;

import graph.module.NLPToStringModule;

public class SemanticArgErrorEdge extends DAGErrorEdge implements
		RetryableErrorEdge {
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
	public String getError(boolean isPretty) {
		if (isPretty) {
			NLPToStringModule nlpModule = (NLPToStringModule) DirectedAcyclicGraph.selfRef_
					.getModule(NLPToStringModule.class);
			if (nlpModule != null)
				return nlpModule.nodeToString(proposedNode_, false)
						+ " is not a valid argument for arg " + argNum_
						+ " of " + nlpModule.nodeToString(predicate_, false)
						+ ".";
		}
		return proposedNode_ + " is not a valid argument for arg " + argNum_
				+ " of " + predicate_ + ".";
	}

	@Override
	public Node[] getNodes() {
		return new Node[] { predicate_, PrimitiveNode.parseNode("" + argNum_),
				proposedNode_ };
	}

}
