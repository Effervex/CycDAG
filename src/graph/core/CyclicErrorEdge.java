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

import graph.inference.QueryObject;
import graph.module.NLPToStringModule;

import org.apache.commons.lang3.StringUtils;

public class CyclicErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private Node[] cyclicAssertion_;

	public CyclicErrorEdge(Node[] assertion) {
		cyclicAssertion_ = assertion;
	}

	@Override
	public String getError(boolean isPretty) {
		if (isPretty) {
			NLPToStringModule nlpModule = (NLPToStringModule) DirectedAcyclicGraph.selfRef_
					.getModule(NLPToStringModule.class);
			if (nlpModule != null)
				return "Asserting '"
						+ nlpModule.edgeToString(new QueryObject(
								cyclicAssertion_), false, false, false, false)
						+ "' would create a cycle.";
		}
		return "Asserting (" + StringUtils.join(cyclicAssertion_, " ")
				+ ") would create a cycle.";
	}

	@Override
	public Node[] getNodes() {
		return cyclicAssertion_;
	}

}
