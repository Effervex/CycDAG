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
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;
import graph.module.TransitiveIntervalSchemaModule;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import org.apache.commons.collections4.CollectionUtils;

public class TransitiveWorker extends QueryWorker {
	private static final long serialVersionUID = -7763389018408199476L;
	private transient TransitiveIntervalSchemaModule transIntModule_;

	public TransitiveWorker(QueryModule queryModule) {
		super(queryModule);
	}

	/**
	 * Runs the transitive interval module to efficiently compute the solution
	 * to a query.
	 * 
	 * @param queryObj
	 *            The query to run and store results in.
	 */
	private void runIntervalModule(QueryObject queryObj) {
		// TODO Ensure node is a collection.
		if (queryObj.isProof()) {
			// Find the proof and justify it.
			Collection<DAGNode> result = transIntModule_.execute(true,
					queryObj.getNode(1), queryObj.getNode(2));
			if (result != null) {
				queryObj.addResult(new Substitution(), queryObj.getNodes());
				List<Node[]> justification = queryObj.getJustification();
				justification.clear();
				justification.addAll(transIntModule_.justifyTransitive(
						(DAGNode) queryObj.getNode(1),
						(DAGNode) queryObj.getNode(2)));
			}
		} else {
			// Find the results and add them to the query object.
			boolean upwards = queryObj.getAtomicIndex() == 1;
			DAGNode baseNode = queryObj.getAtomic();
			Collection<DAGNode> transitiveNodes = transIntModule_.execute(
					upwards, baseNode);
			if (transitiveNodes == null)
				return;
			for (DAGNode n : transitiveNodes) {
				queryObj.addCompleted(n);
				Node[] nodes = Arrays.copyOf(queryObj.getNodes(), 3);
				nodes[queryObj.getVariableIndex()] = n;
				queryObj.addResult(nodes);
			}
		}
	}

	protected void transitiveSearch(QueryObject queryObj) {
		// Find the index
		int atomicIndex = queryObj.getAtomicIndex();
		int varIndex = (atomicIndex == 1) ? 2 : 1;
		DAGNode atomic = queryObj.getAtomic();
		if (atomic == null)
			return;

		Queue<DAGNode> toCheck = new LinkedList<>();
		toCheck.add(atomic);
		Collection<Edge> genlEdges = relatedModule_
				.findEdgeByNodes((DAGNode) queryObj.getNode(0));

		while (!toCheck.isEmpty()) {
			DAGNode n = querier_.getExpanded(toCheck.poll());

			if (queryObj.isCompleted(n))
				continue;
			queryObj.addCompleted(n);

			// Function checking
			if (atomicIndex == 1 && n instanceof OntologyFunction) {
				Collection<DAGNode> functionEdges = querier_.functionResults(
						(OntologyFunction) n, CommonConcepts.RESULT_GENL);
				for (DAGNode resultNode : functionEdges) {
					if (queryObj.isProof()
							&& resultNode.equals(queryObj.getNode(2))) {
						queryObj.addResult(new Substitution(),
								CommonConcepts.GENLS.getNode(dag_), n,
								resultNode);
						return;
					}
					if (queryObj.addResult(CommonConcepts.GENLS.getNode(dag_),
							n, resultNode))
						return;
					toCheck.add(resultNode);
				}
			}

			// Intersect the collections
			Collection<Edge> nodeEdges = relatedModule_.execute(n,
					atomicIndex + 1);
			nodeEdges = CollectionUtils.retainAll(nodeEdges, genlEdges);

			// Self genls check
			if (n == atomic) {
				Collection<Edge> selfEdges = nodeEdges;
				if (selfEdges.isEmpty()) {
					selfEdges = relatedModule_.execute(atomic, varIndex + 1);
					selfEdges = CollectionUtils.retainAll(selfEdges, genlEdges);
				}
				if (!selfEdges.isEmpty()) {
					if (queryObj.addResult(queryObj.getNode(0), atomic, n))
						return;
				}
			}

			// Create the subs
			for (Edge e : nodeEdges) {
				if (!(e.getNodes()[varIndex] instanceof DAGNode))
					continue;
				DAGNode edgeNode = (DAGNode) e.getNodes()[varIndex];

				if (queryObj.isProof()
						&& e.getNodes()[varIndex].equals(queryObj.getNode(2))) {
					queryObj.addResult(new Substitution(), e.getNodes());
					return;
				}
				if (queryObj.addResult(e.getNodes()))
					return;
				toCheck.add(edgeNode);
			}
		}
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// Use the interval module if available
		if (transIntModule_ != null
				&& transIntModule_.isReady()
				&& transIntModule_.getTransitiveNode().equals(
						queryObj.getNode(0))) {
			runIntervalModule(queryObj);
		} else {
			transitiveSearch(queryObj);

			if (queryObj.isProof())
				queryObj.cleanTransitiveJustification(queryObj
						.getJustification());
		}
	}

	@Override
	public void setDAG(DirectedAcyclicGraph dag) {
		super.setDAG(dag);
		transIntModule_ = (TransitiveIntervalSchemaModule) dag_
				.getModule(TransitiveIntervalSchemaModule.class);
		if (transIntModule_ == null)
			System.out.println("Warning: QueryModule is more efficient "
					+ "with Transitive Interval Schema Module active. "
					+ "It is recommended to restart with the "
					+ "Transitive Interval Schema Module active.");
	}
}
