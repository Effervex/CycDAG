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
import graph.core.EdgeModifier;
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

	private boolean canUseTransitiveModule(QueryObject queryObj) {
		Node[] nodes = queryObj.getNodes();
		if (transIntModule_ == null || !transIntModule_.isReady()
				|| !transIntModule_.getTransitiveNode().equals(nodes[0]))
			return false;
		// Make sure all nodes are present in the transitive interval module.
		for (int i = 1; i < nodes.length; i++)
			if (nodes[i] instanceof DAGNode
					&& ((DAGNode) nodes[i])
							.getProperty(TransitiveIntervalSchemaModule.PREDECESSOR_ID) == null)
				return false;
		// Otherwise, check it's ready
		return true;
	}

	/**
	 * Runs the transitive interval module to efficiently compute the solution
	 * to a query.
	 * 
	 * @param queryObj
	 *            The query to run and store results in.
	 */
	private void runIntervalModule(QueryObject queryObj) {
		if (queryObj.isProof()) {
			// Find the proof and justify it.
			Collection<DAGNode> result = transIntModule_.execute(true,
					queryObj.getNode(1), queryObj.getNode(2));
			if (result != null) {
				queryObj.addResult(true, new Substitution(),
						queryObj.getNodes());
				if (queryObj.shouldJustify()) {
					List<Node[]> justification = queryObj.getJustification();
					justification.clear();
					justification.addAll(transIntModule_.justifyTransitive(
							(DAGNode) queryObj.getNode(1),
							(DAGNode) queryObj.getNode(2)));
				}
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
				queryObj.addResult(true, nodes);
			}
		}
	}

	protected void transitiveSearch(QueryObject queryObj) {
		// Find the index
		int atomicIndex = queryObj.getAtomicIndex();
		int varIndex = (atomicIndex == 1) ? 2 : 1;
		DAGNode atomic = queryObj.getAtomic();
		Node[] queryNodes = queryObj.getNodes();
		if (atomic == null)
			return;

		Queue<DAGNode> toCheck = new LinkedList<>();
		toCheck.add(atomic);
		Collection<Edge> genlEdges = relatedModule_
				.findEdgeByNodes((DAGNode) queryNodes[0]);

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
							&& resultNode.equals(queryNodes[2])) {
						queryObj.addResult(true, new Substitution(),
								CommonConcepts.GENLS.getNode(dag_), n,
								resultNode);
						return;
					}
					if (queryObj.addResult(true,
							CommonConcepts.GENLS.getNode(dag_), n, resultNode))
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
					if (queryObj
							.addResult(true, queryNodes[0], atomic, n))
						return;
				}
			}

			// Create the subs
			for (Edge e : nodeEdges) {
				if (EdgeModifier.isRemoved(e, dag_))
					continue;
				DAGNode edgeNode = (DAGNode) EdgeModifier
						.getUnmodNodes(e, dag_)[varIndex];
				if (!(edgeNode instanceof DAGNode))
					continue;

				// If the node matches the query (or variable), add a result
				boolean negated = EdgeModifier.isNegated(e, dag_);
				if (queryObj.isProof() && edgeNode.equals(queryNodes[2])) {
					queryObj.addResult(!negated, new Substitution(),
							e.getNodes());
					return;
				} else if (!negated && queryObj.addResult(true, e.getNodes()))
					return;
				toCheck.add(edgeNode);
			}
		}
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// Use the interval module if available
		if (canUseTransitiveModule(queryObj)) {
			// TODO Deal with negation here
			runIntervalModule(queryObj);
		} else {
			transitiveSearch(queryObj);

			if (queryObj.isProof())
				queryObj.cleanTransitiveJustification(
						queryObj.getJustification(), dag_);
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
