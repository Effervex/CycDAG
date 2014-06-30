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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;

/**
 * Keeps track of all rewriteOf assertions and provides quick access to
 * rewritten nodes.
 * 
 * @author Sam Sarjant
 */
public class RewriteOfModule extends DAGModule<DAGNode> {
	private static final long serialVersionUID = 1L;
	private Map<DAGNode, DAGNode> rewriteMap_;
	private transient RelatedEdgeModule relEdgeModule_;

	public RewriteOfModule() {
		rewriteMap_ = new HashMap<>();
	}

	@Override
	public DAGNode execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (args == null || args.length < 1)
			return null;
		DAGNode node = (DAGNode) args[0];
		DAGNode rewrite = rewriteMap_.get(node);
		if (rewrite != null)
			return rewrite;
		return node;
	}

	@Override
	public boolean initialisationComplete(Collection<DAGNode> nodes,
			Collection<DAGEdge> edges, boolean forceRebuild) {
		if (forceRebuild) {
			defaultRebuild(nodes, false, edges, true);
			return true;
		}
		return false;
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		if (relEdgeModule_ == null)
			relEdgeModule_ = (RelatedEdgeModule) dag_
					.getModule(RelatedEdgeModule.class);
		Node[] edgeNodes = edge.getNodes();

		if (edgeNodes[0].equals(CommonConcepts.REWRITE_OF.getNode(dag_))) {
			// No rewrite
			if (edgeNodes[1].equals(edgeNodes[2]))
				return false;

			Node creator = null;
			if (edge.getCreator() != null)
				creator = dag_.findOrCreateNode(edge.getCreator(), null, false);
			boolean ephemeral = edge
					.getProperty(DirectedAcyclicGraph.EPHEMERAL_MARK) != null;

			// Check it's not already in the rewrite map
			if (rewriteMap_.containsKey(edgeNodes[1])) {
				DAGNode favoured = rewriteMap_.get(edgeNodes[1]);
				if (!edgeNodes[2].equals(favoured)) {
					rewriteMap_.put((DAGNode) edgeNodes[2], favoured);
					Node[] rewrittenEdge = Arrays.copyOf(edgeNodes,
							edgeNodes.length);
					rewrittenEdge[1] = favoured;
					dag_.findOrCreateEdge(rewrittenEdge, creator, true,
							ephemeral, true);
					return false;
				} else {
					// Go with the later edge
					Edge oppositeRewrite = dag_.findOrCreateEdge(new Node[] {
							CommonConcepts.REWRITE_OF.getNode(dag_),
							edgeNodes[2], edgeNodes[1] }, null, false);
					if (oppositeRewrite != null
							&& !(oppositeRewrite instanceof ErrorEdge)) {
						dag_.removeEdge(oppositeRewrite);
					} else
						return false;
				}
			}

			// Add to rewritten map
			rewriteMap_.put((DAGNode) edgeNodes[2], (DAGNode) edgeNodes[1]);

			// Go through prior edges and assert for rewritten node
			Collection<Edge> associatedEdges = relEdgeModule_
					.execute(edgeNodes[2]);

			// Propagate all edges for the rewritten node forward to the target.
			for (Edge e : associatedEdges) {
				if (e.getNodes()[0].equals(CommonConcepts.REWRITE_OF
						.getNode(dag_)))
					continue;
				// Alter the edge nodes
				Node[] rewrittenEdgeNodes = Arrays.copyOf(e.getNodes(),
						e.getNodes().length);
				for (int i = 0; i < rewrittenEdgeNodes.length; i++)
					if (rewrittenEdgeNodes[i].equals(edgeNodes[2]))
						rewrittenEdgeNodes[i] = edgeNodes[1];
				Edge rewrittenEdge = dag_.findOrCreateEdge(rewrittenEdgeNodes,
						creator, true, ephemeral, true);

				if (rewrittenEdge == null || rewrittenEdge instanceof ErrorEdge)
					System.out.println("Could not rewrite edge " + e + ": "
							+ rewrittenEdge);
				else
					// Remove the associated edge
					dag_.removeEdge(e);
			}
		} else {
			Node[] rewrittenEdgeNodes = edgeNodes;
			for (int i = 0; i < edgeNodes.length; i++) {
				if (rewriteMap_.containsKey(rewrittenEdgeNodes[i])) {
					// An edge needs rewriting
					if (rewrittenEdgeNodes == edgeNodes) {
						rewrittenEdgeNodes = Arrays.copyOf(edgeNodes,
								edgeNodes.length);
					}
					rewrittenEdgeNodes[i] = rewriteMap_
							.get(rewrittenEdgeNodes[i]);
					i--;
				}
			}

			Node creator = null;
			if (edge.getCreator() != null)
				creator = dag_.findOrCreateNode(edge.getCreator(), null, false);
			boolean ephemeral = edge
					.getProperty(DirectedAcyclicGraph.EPHEMERAL_MARK) != null;

			if (rewrittenEdgeNodes != edgeNodes) {
				// Assert rewritten edge
				Edge rewrittenEdge = dag_.findOrCreateEdge(rewrittenEdgeNodes,
						creator, true, ephemeral, true);
				if (rewrittenEdge == null || rewrittenEdge instanceof ErrorEdge) {
					System.out.println("Could not rewrite edge " + edge + ": "
							+ rewrittenEdge);
					return true;
				}
				// Do not accept the edge
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean removeEdge(DAGEdge edge) {
		if (edge.getNodes()[0].equals(CommonConcepts.REWRITE_OF.getNode(dag_))) {
			rewriteMap_.remove(edge.getNodes()[2]);
		}
		return super.removeEdge(edge);
	}

	@Override
	public String toString() {
		return "RewriteOf Module: " + rewriteMap_.size()
				+ " rewritten concepts";
	}

	public DAGNode getRewrite(DAGNode node) {
		return execute(node);
	}
}
