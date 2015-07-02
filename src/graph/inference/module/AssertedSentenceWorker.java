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
import graph.core.Edge;
import graph.core.EdgeModifier;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.QueryWorker;
import graph.inference.VariableNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;

public class AssertedSentenceWorker extends QueryWorker {

	private static final long serialVersionUID = -1648137430706821896L;

	public AssertedSentenceWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private Object[] queryAsIndexedNodes(Node... args) {
		ArrayList<Object> relatedArgs = new ArrayList<>();
		for (int i = 0; i < args.length; i++) {
			if (!(args[i] instanceof VariableNode)) {
				relatedArgs.add(args[i]);
				relatedArgs.add(i + 1);
			}
		}
		return relatedArgs.toArray(new Object[relatedArgs.size()]);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// Modify to remove assertedSentence prefix
		if (queryObj.getNode(0).equals(
				CommonConcepts.ASSERTED_SENTENCE.getNode(dag_))) {
			OntologyFunction assertion = (OntologyFunction) queryObj.getNode(1);
			queryObj = queryObj.modifyNodes(assertion.getNodes());
		}

		// Find edges by name and index.
		Object[] relatedArgs = queryAsIndexedNodes(queryObj.getNodes());
		if (relatedArgs.length == 0)
			throw new IllegalArgumentException(
					"Query must include at least one atomic node.");
		Collection<Edge> edges = relatedModule_.execute(relatedArgs);
		for (Edge edge : edges) {
			if (queryObj.addResult(!EdgeModifier.isSpecial(edge, dag_),
					edge.getNodes()))
				return;
		}

		boolean isSymmetric = querier_.prove(false,
				CommonConcepts.ISA.getNode(dag_), queryObj.getNode(0),
				CommonConcepts.SYMMETRIC_BINARY.getNode(dag_)) == QueryResult.TRUE;
		if (isSymmetric) {
			relatedArgs = queryAsIndexedNodes(queryObj.getNode(0),
					queryObj.getNode(2), queryObj.getNode(1));
			edges = relatedModule_.execute(relatedArgs);

			for (Edge edge : edges) {
				if (EdgeModifier.isRemoved(edge, dag_))
					continue;
				Node[] edgeNodes = EdgeModifier.getUnmodNodes(edge, dag_);
				boolean negated = EdgeModifier.isNegated(edge, dag_);
				Node[] negNodes = (negated) ? new Node[] {
						CommonConcepts.NOT.getNode(dag_),
						new OntologyFunction(edgeNodes[0], edgeNodes[2],
								edgeNodes[1]) } : new Node[] { edgeNodes[0],
						edgeNodes[2], edgeNodes[1] };
				if (queryObj.addResult(!negated, negNodes))
					return;
			}
		}
	}
}
