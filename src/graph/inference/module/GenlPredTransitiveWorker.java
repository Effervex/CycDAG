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
import graph.core.Node;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Arrays;
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
		ArrayList<Object> nonPreds = new ArrayList<>();
		Node[] nodes = queryObj.getNodes();
		for (int i = 1; i < nodes.length; i++) {
			if (!(nodes[i] instanceof VariableNode)) {
				nonPreds.add(nodes[i]);
				nonPreds.add(i + 1);
			}
		}
		Collection<Edge> nonPredEdges = relatedModule_.execute(nonPreds
				.toArray(new Object[nonPreds.size()]));

		// Sub preds
		VariableNode varNode = new VariableNode("?SUB_PREDS");
		Collection<Substitution> subPreds = querier_.execute(
				CommonConcepts.GENLPREDS.getNode(dag_), varNode,
				queryObj.getNode(0));
		if (subPreds.isEmpty())
			subPreds.add(new Substitution(varNode, (DAGNode) queryObj
					.getNode(0)));
		for (Substitution sub : subPreds) {
			Node subPred = sub.getSubstitution(varNode);
			Collection<Edge> predEdges = relatedModule_
					.findEdgeByNodes(subPred);
			Collection<Edge> intersect = CollectionUtils.retainAll(
					nonPredEdges, predEdges);
			for (Edge interEdge : intersect) {
				Node[] edgeNodes = Arrays.copyOf(interEdge.getNodes(),
						interEdge.getNodes().length);
				if (queryObj.addResult(edgeNodes))
					return;
			}
		}
	}
}
