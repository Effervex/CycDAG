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

public class ArityErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private String numArgs_;
	private DAGNode predicate_;

	public ArityErrorEdge(DAGNode predicateNode, String numArgs) {
		predicate_ = predicateNode;
		numArgs_ = numArgs; 
	}

	@Override
	public String getError(boolean isPretty) {
		if (isPretty) {
			NLPToStringModule nlpModule = (NLPToStringModule) DirectedAcyclicGraph.selfRef_
					.getModule(NLPToStringModule.class);
			if (nlpModule != null)
				return "Predicate " + nlpModule.nodeToString(predicate_, false)
						+ " should only have " + numArgs_ + " arguments.";
		}
		return "Predicate " + predicate_ + " should only have " + numArgs_
				+ " arguments.";
	}

	@Override
	public Node[] getNodes() {
		return new Node[] { predicate_ };
	}
}
