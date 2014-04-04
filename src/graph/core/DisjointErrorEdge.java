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
import graph.core.DAGErrorEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;

public class DisjointErrorEdge extends DAGErrorEdge implements RetryableErrorEdge {
	private static final long serialVersionUID = 3921728486130853014L;
	private Node[] disjointNodes_;

	public DisjointErrorEdge(DAGNode newCollection, DAGNode existingCollection,
			DirectedAcyclicGraph dag) {
		disjointNodes_ = new Node[3];
		disjointNodes_[0] = CommonConcepts.DISJOINTWITH.getNode(dag);
		disjointNodes_[1] = newCollection;
		disjointNodes_[2] = existingCollection;
	}

	@Override
	public String getError() {
		return disjointNodes_[1] + " is disjoint with asserted collection "
				+ disjointNodes_[2];
	}

	@Override
	public Node[] getNodes() {
		return disjointNodes_;
	}
}
