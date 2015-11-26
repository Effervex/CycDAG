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
package graph.module;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.EdgeModifier;
import graph.core.Node;

import java.util.Collection;
import java.util.HashSet;

import util.collection.trove.TIndexedCollection;

public class StrictlyFunctionalModule extends DAGModule<DAGNode> {
	private static final long serialVersionUID = 1L;
	private Collection<DAGNode> strictlyFunctionalPreds_;
	private transient RelatedEdgeModule relEdgeModule_;

	public StrictlyFunctionalModule() {
	}

	@Override
	public DAGNode execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		return null;
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		if (strictlyFunctionalPreds_ == null)
			strictlyFunctionalPreds_ = new HashSet<>();
		Node[] edgeNodes = edge.getNodes();

		// Check if the edge is a new strictly functional predicate
		if (edgeNodes[0] == CommonConcepts.ISA.getNode(dag_)
				&& edgeNodes[2] == CommonConcepts.STRICTLY_FUNCTIONAL_SLOT
						.getNode(dag_)) {
			strictlyFunctionalPreds_.add((DAGNode) edgeNodes[1]);
			return true;
		}

		// Check if the predicate is strictly functional
		if (strictlyFunctionalPreds_.contains(edgeNodes[0])) {
			if (relEdgeModule_ == null)
				relEdgeModule_ = (RelatedEdgeModule) dag_
						.getModule(RelatedEdgeModule.class);

			// Check if the first argument already has an edge
			Collection<Edge> existing = relEdgeModule_.findEdgeByNodes(
					edgeNodes[0], edgeNodes[1]);

			// Remove all existing edges (hopefully only one)
			for (Edge e : existing) {
				if (!edge.equals(e))
					((CycDAG) dag_).removeEdge(e, false);
			}
			return true;
		}
		return super.addEdge(edge);
	}

	@Override
	public boolean initialisationComplete(TIndexedCollection<DAGNode> nodes,
			TIndexedCollection<DAGEdge> edges, boolean forceRebuild) {
		if (forceRebuild || strictlyFunctionalPreds_ == null) {
			defaultRebuild(nodes, false, edges, true);
			return true;
		}
		return false;
	}

	@Override
	public String toString() {
		return "Strictly Functional Module: " + strictlyFunctionalPreds_.size()
				+ " strictly functional predicates";
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return !EdgeModifier.isSpecial(edge, dag_);
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}
}
