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
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.DisjointModule;
import graph.module.QueryModule;
import graph.module.TransitiveIntervalSchemaModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;

public class DisjointWithWorker extends QueryWorker {
	private static final long serialVersionUID = -2014914913163958118L;
	private transient DisjointModule disjointModule_;

	public DisjointWithWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private Collection<? extends Node[]> alterGenlJustification(
			QueryObject genlResults, DAGNode disjointNode, boolean isReversed) {
		TransitiveIntervalSchemaModule transModule = (TransitiveIntervalSchemaModule) dag_
				.getModule(TransitiveIntervalSchemaModule.class);
		if (transModule != null && transModule.isReady()) {
			DAGNode baseNode = (DAGNode) genlResults.getNode(1);
			List<Node[]> justification = transModule.justifyTransitive(
					baseNode, disjointNode);
			if (isReversed)
				Collections.reverse(justification);
			return justification;
		}

		List<Node[]> justifications = genlResults.getJustification();
		int i = 0;
		for (i = 0; i < justifications.size(); i++) {
			Node[] nodes = justifications.get(i);
			if (nodes[1].equals(disjointNode))
				break;
			if (nodes[2].equals(disjointNode)) {
				i++;
				break;
			}
		}
		justifications = justifications.subList(0, i);
		genlResults.cleanTransitiveJustification(justifications, dag_);

		if (isReversed) {
			List<Node[]> reversed = new ArrayList<>(
					genlResults.getJustification());
			Collections.reverse(reversed);
			return reversed;
		}
		return genlResults.getJustification();
	}

	private void checkSiblingDisjoint(QueryObject queryObj) {

		// If the concept part of a sibling disjoint collection(s)
		DAGNode atomic = queryObj.getAtomic();
		if (atomic == null)
			return;

		VariableNode queryVar = new VariableNode("?_DISJ_");
		VariableNode transOne = new VariableNode("?_TRANSONE_");
		VariableNode transTwo = new VariableNode("?_TRANSTWO_");
		Node node1 = queryObj.getNode(1);
		Node node2 = queryObj.getNode(2);
		QueryObject qo = new QueryObject(true, queryObj.shouldJustify(),
				CommonConcepts.AND.getNode(dag_), new OntologyFunction(
						CommonConcepts.ISA.getNode(dag_), node1, queryVar), // First
				new OntologyFunction(CommonConcepts.ISA.getNode(dag_), node2,
						queryVar), // Second
				new OntologyFunction(CommonConcepts.ISA.getNode(dag_),
						queryVar,
						CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
								.getNode(dag_)) // Is a sibling disjoint
		);
		Collection<Substitution> siblingDisjoints = querier_.executeQuery(qo);

		if (queryObj.isProof() && qo.getResultState() == QueryResult.TRUE) {
			// Sibling Disjoint Exception?
			boolean isException = isException(node1, node2);
			Substitution sub = siblingDisjoints.iterator().next();
			List<Node[]> justification = null;
			if (queryObj.shouldJustify()) {
				justification = processSiblingJustification(
						sub.getSubstitution(transOne),
						sub.getSubstitution(transTwo),
						sub.getSubstitution(queryVar), queryObj);

				// If exception, add in exception to justification
				if (isException)
					justification.add(new Node[] {
							CommonConcepts.SIBLING_DISJOINT_EXCEPTION
									.getNode(dag_), node1, node2 });
			}
			queryObj.addResult(!isException, (isException) ? null
					: new Substitution(), justification);
		} else {
			for (Substitution sub : siblingDisjoints) {
				VariableNode varNode = queryObj.getVariable();
				for (Node sibling : CommonQuery.INSTANCES.runQuery(dag_,
						sub.getSubstitution(queryVar))) {
					if (!sibling.equals(atomic)
							&& !isException(atomic, sibling))
						queryObj.addResult(true, new Substitution(varNode,
								(DAGNode) sibling), sub
								.getSubstitution(queryVar),
								CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
										.getNode(dag_));
				}
			}
		}
	}

	private void findDisjoint(QueryObject queryObj,
			Collection<Edge> disjointWithEdges) {
		if (queryObj.getAtomic() == null)
			return;
		VariableNode varNode = (queryObj.isProof()) ? VariableNode.DEFAULT
				: queryObj.getVariable();
		QueryObject genlResults1 = new QueryObject(false,
				queryObj.shouldJustify(), CommonConcepts.GENLS.getNode(dag_),
				queryObj.getAtomic(), varNode);
		querier_.executeQuery(genlResults1);
		QueryObject genlResults2 = null;

		Collection<DAGNode> genlResults = genlResults1.getCompleted();
		Collection<DAGNode> larger = null;
		boolean swapped = false;

		// For proofs, work out the smaller set of the two.
		if (queryObj.isProof()) {
			genlResults2 = new QueryObject(false, queryObj.shouldJustify(),
					CommonConcepts.GENLS.getNode(dag_), queryObj.getNode(2),
					varNode);
			querier_.executeQuery(genlResults2);

			genlResults = new ArrayList<>(genlResults1.getCompleted());
			genlResults.removeAll(genlResults2.getCompleted());
			larger = new ArrayList<>(genlResults2.getCompleted());
			larger.removeAll(genlResults1.getCompleted());
			if (genlResults.size() > larger.size()) {
				Collection<DAGNode> temp = genlResults;
				genlResults = larger;
				larger = temp;
				swapped = true;
			}

			// Not disjoint
			if (genlResults.isEmpty())
				return;
			larger = new HashSet<>(larger);
		}

		for (DAGNode nodeA : genlResults) {
			// Check if the node has any disjointWith assertions that are in the
			// other genls
			Collection<Edge> node1Edges = relatedModule_.execute(nodeA);
			node1Edges = CollectionUtils.retainAll(node1Edges,
					disjointWithEdges);
			for (Edge e : node1Edges) {
				if (EdgeModifier.isRemoved(e, dag_))
					continue;
				Node[] edgeNodes = EdgeModifier.getUnmodNodes(e, dag_);
				Node thisNode, otherNode = null;
				if (edgeNodes[1].equals(nodeA)) {
					thisNode = edgeNodes[1];
					otherNode = edgeNodes[2];
				} else if (edgeNodes[2].equals(nodeA)) {
					thisNode = edgeNodes[2];
					otherNode = edgeNodes[1];
				} else
					continue;

				if (queryObj.isProof() && larger.contains(otherNode)) {
					// Disjoint found!
					// Add genl justifications either side.
					if (swapped) {
						Node temp = thisNode;
						thisNode = otherNode;
						otherNode = temp;
					}
					if (queryObj.shouldJustify())
						queryObj.getJustification().addAll(
								alterGenlJustification(genlResults1,
										(DAGNode) thisNode, false));
					queryObj.addResult(!EdgeModifier.isNegated(e, dag_),
							new Substitution(), e.getNodes());
					if (queryObj.shouldJustify())
						queryObj.getJustification().addAll(
								alterGenlJustification(genlResults2,
										(DAGNode) otherNode, true));

					return;
				} else if (!queryObj.isProof())
					queryObj.addResult(!EdgeModifier.isNegated(e, dag_),
							new Substitution(varNode, (DAGNode) otherNode),
							e.getNodes());
			}
		}
	}

	private boolean isException(Node node, Node node2) {
		QueryObject isException = new QueryObject(
				CommonConcepts.SIBLING_DISJOINT_EXCEPTION.getNode(dag_), node,
				node2);
		querier_.executeQuery(isException);
		return isException.getResultState() == QueryResult.TRUE;
	}

	/**
	 * Checking there are no anonymous nodes in the disjoint query - the
	 * disjoint module can't cope with them.
	 * 
	 * @param queryObj
	 *            The query object to check.
	 * @return True if the query does not contain any anonymous nodes.
	 */
	private boolean notAnonymous(QueryObject queryObj) {
		Node[] nodes = queryObj.getNodes();
		for (int i = 1; i < nodes.length; i++) {
			if (nodes[i] instanceof DAGNode
					&& ((DAGNode) nodes[i]).isAnonymous())
				return false;
		}
		return true;
	}

	/**
	 * Processes the justification for the sibling.
	 * 
	 * @param substitution
	 *            The substitution for the variables such that the sibling
	 *            disjoint query is satisfied.
	 * @param transOne
	 *            The transitive variable for the first object.
	 * @param transTwo
	 *            The transitive variable for the second object.
	 * @param queryVar
	 *            The variable linking transOne and transTwo to the
	 *            SiblingDisjointCollection.
	 * @param queryObj
	 *            The query object to add the justification to.
	 * 
	 */
	private List<Node[]> processSiblingJustification(Node transOne,
			Node transTwo, Node queryVar, QueryObject queryObj) {
		List<Node[]> justification = new ArrayList<>();
		QueryObject proof = null;

		// Add transitive query 1, unless it is the same
		Node arg1 = queryObj.getNode(1);

		// Add isa sibling query for arg 1
		proof = new QueryObject(false, queryObj.shouldJustify(),
				CommonConcepts.ISA.getNode(dag_), arg1, queryVar);
		querier_.prove(proof);
		if (queryObj.shouldJustify()) {
			justification.addAll(proof.getJustification());
			justification.add(new Node[0]);
		}

		// Add transitive query 2, unless it is the same
		Node arg2 = queryObj.getNode(2);

		// Add isa sibling query for arg 2
		proof = new QueryObject(false, queryObj.shouldJustify(),
				CommonConcepts.ISA.getNode(dag_), arg2, queryVar);
		querier_.prove(proof);
		if (queryObj.shouldJustify()) {
			justification.addAll(proof.getJustification());
			justification.add(new Node[0]);
		}

		// Add isa sibling collection to sibling collection type
		proof = new QueryObject(false, queryObj.shouldJustify(),
				CommonConcepts.ISA.getNode(dag_), queryVar,
				CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE.getNode(dag_));
		querier_.prove(proof);
		if (queryObj.shouldJustify()) {
			justification.addAll(proof.getJustification());
		}
		return justification;
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// First check if either one is a child of the other
		if (transitiveChildren(queryObj))
			return;

		// Use disjoint module if possible
		if (disjointModule_ != null && notAnonymous(queryObj)) {
			disjointModule_.execute(queryObj);
		} else {
			// Disjoint edges
			Collection<Edge> disjointWithEdges = relatedModule_
					.findEdgeByNodes(CommonConcepts.DISJOINTWITH.getNode(dag_));

			// Find direct disjoints
			findDisjoint(queryObj, disjointWithEdges);
		}

		if (queryObj.isProof() && queryObj.getResultState() != QueryResult.NIL)
			return;

		// Find sibling disjoints
		checkSiblingDisjoint(queryObj);
	}

	private boolean transitiveChildren(QueryObject queryObj) {
		// Skip if variables involved.
		if (queryObj.getVariable() != null)
			return false;
		QueryObject genlsA = new QueryObject(false, queryObj.shouldJustify(),
				CommonConcepts.GENLS.getNode(dag_), queryObj.getNode(1),
				queryObj.getNode(2));
		if (querier_.prove(genlsA) == QueryResult.TRUE) {
			queryObj.addResult(false, null, genlsA.getJustification());
			return true;
		}
		QueryObject genlsB = new QueryObject(false, queryObj.shouldJustify(),
				CommonConcepts.GENLS.getNode(dag_), queryObj.getNode(2),
				queryObj.getNode(1));
		if (querier_.prove(genlsB) == QueryResult.TRUE) {
			queryObj.addResult(false, null, genlsB.getJustification());
			return true;
		}
		return false;
	}

	@Override
	public void setDAG(DirectedAcyclicGraph dag) {
		super.setDAG(dag);
		disjointModule_ = (DisjointModule) dag_.getModule(DisjointModule.class);
		if (disjointModule_ == null)
			System.out.println("Warning: Disjointness calculations "
					+ "are more efficient with Disjoint Module active. "
					+ "It is recommended to restart with the Disjoint "
					+ "Module active.");
	}
}
