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
