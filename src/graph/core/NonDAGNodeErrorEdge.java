/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *    Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.core;

public class NonDAGNodeErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private Node[] edgeNodes_;
	
	public NonDAGNodeErrorEdge(Node[] edgeNodes) {
		edgeNodes_ = edgeNodes;
	}

	@Override
	public String getError(boolean isPretty) {
		return "Edge cannot have non-DAG node as first argument!";
	}

	@Override
	public Node[] getNodes() {
		return edgeNodes_;
	}

}
