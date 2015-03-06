/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.inference.module;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.EdgeModifier;
import graph.core.Node;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.collections4.CollectionUtils;

public class GenlPredTransitiveWorker extends QueryWorker {
	public GenlPredTransitiveWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private static final long serialVersionUID = -7782457322494540617L;

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// Find related edges for args
		Node[] nodes = queryObj.getNodes();
		Collection<Edge> nonPredEdges = getRelatedEdges(nodes);

		// If not complete, try symmetric
		boolean isSymmetric = querier_.prove(false,
				CommonConcepts.ISA.getNode(dag_), queryObj.getNode(0),
				CommonConcepts.SYMMETRIC_BINARY.getNode(dag_)) == QueryResult.TRUE;
		if (isSymmetric)
			nonPredEdges.addAll(getRelatedEdges(new Node[] { nodes[0],
					nodes[2], nodes[1] }));

		// Sub preds
		VariableNode varNode = new VariableNode("?SUB_PREDS");
		Collection<Substitution> subPreds = querier_
				.executeQuery(new QueryObject(CommonConcepts.GENLPREDS
						.getNode(dag_), varNode, queryObj.getNode(0)));
		if (subPreds.isEmpty())
			subPreds.add(new Substitution(varNode, (DAGNode) queryObj
					.getNode(0)));

		// Try the subs
		for (Substitution sub : subPreds) {
			Node subPred = sub.getSubstitution(varNode);
			Collection<Edge> predEdges = relatedModule_
					.findEdgeByNodes(subPred);
			Collection<Edge> intersect = CollectionUtils.retainAll(
					nonPredEdges, predEdges);
			for (Edge interEdge : intersect) {
				Node[] edgeNodes = interEdge.getNodes();
				if (queryObj.addResult(
						!EdgeModifier.isNegated(interEdge, dag_), edgeNodes))
					return;
				if (isSymmetric
						&& queryObj.addResult(
								!EdgeModifier.isNegated(interEdge, dag_),
								new Node[] { edgeNodes[0], edgeNodes[2],
										edgeNodes[1] }))
					return;
			}
		}
	}

	protected Collection<Edge> getRelatedEdges(Node[] nodes) {
		ArrayList<Object> nonPreds = new ArrayList<>();
		for (int i = 1; i < nodes.length; i++) {
			if (!(nodes[i] instanceof VariableNode)) {
				nonPreds.add(nodes[i]);
				nonPreds.add(i + 1);
			}
		}
		Collection<Edge> nonPredEdges = relatedModule_.execute(nonPreds
				.toArray(new Object[nonPreds.size()]));
		return nonPredEdges;
	}
}
