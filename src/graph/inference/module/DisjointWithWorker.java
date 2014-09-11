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
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.DisjointModule;
import graph.module.QueryModule;
import graph.module.SiblingDisjointModule;
import graph.module.TransitiveIntervalSchemaModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;

public class DisjointWithWorker extends QueryWorker {
	private static final long serialVersionUID = -2014914913163958118L;
	private transient SiblingDisjointModule sibModule_;
	private transient DisjointModule disjointModule_;

	public DisjointWithWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private void findDisjoint(QueryObject queryObj,
			Collection<Edge> disjointWithEdges) {
		if (queryObj.getAtomic() == null)
			return;
		VariableNode varNode = (queryObj.isProof()) ? VariableNode.DEFAULT
				: queryObj.getVariable();
		QueryObject genlResults1 = new QueryObject(
				CommonConcepts.GENLS.getNode(dag_), queryObj.getAtomic(),
				varNode);
		querier_.applyModule("genls", genlResults1);
		QueryObject genlResults2 = null;

		Collection<DAGNode> genlResults = genlResults1.getCompleted();
		Collection<DAGNode> larger = null;
		boolean swapped = false;

		// For proofs, work out the smaller set of the two.
		if (queryObj.isProof()) {
			genlResults2 = new QueryObject(CommonConcepts.GENLS.getNode(dag_),
					queryObj.getNode(2), varNode);
			querier_.applyModule("genls", genlResults2);

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
				Node[] edgeNodes = e.getNodes();
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
					queryObj.addResult(new Substitution(), edgeNodes);
					if (queryObj.shouldJustify())
						queryObj.getJustification().addAll(
								alterGenlJustification(genlResults2,
										(DAGNode) otherNode, true));

					return;
				} else if (!queryObj.isProof())
					queryObj.addResult(new Substitution(varNode,
							(DAGNode) otherNode), edgeNodes);
			}
		}
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
		genlResults.cleanTransitiveJustification(justifications);

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

		// Use sibling module if available
		if (sibModule_ != null && sibModule_.isReady()) {
			siblingDisjointViaModule(atomic, queryObj);
			return;
		}

		VariableNode queryVar = new VariableNode("?_DISJ_");
		VariableNode transOne = new VariableNode("?_TRANSONE_");
		VariableNode transTwo = new VariableNode("?_TRANSTWO_");
		QueryObject qo = new QueryObject(CommonConcepts.AND.getNode(dag_),
				new OntologyFunction(CommonConcepts.GENLS.getNode(dag_),
						queryObj.getNode(1), transOne), // Transitive first arg
				new OntologyFunction(CommonConcepts.ISA.getNode(dag_),
						transOne, queryVar), // First
				new OntologyFunction(CommonConcepts.GENLS.getNode(dag_),
						queryObj.getNode(2), transTwo), // Transitive second arg
				new OntologyFunction(CommonConcepts.ISA.getNode(dag_),
						transTwo, queryVar), // Second
				new OntologyFunction(CommonConcepts.ISA.getNode(dag_),
						queryVar,
						CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
								.getNode(dag_)));
		Collection<Substitution> siblingDisjoints = querier_.execute(qo);

		if (queryObj.isProof() && !siblingDisjoints.isEmpty()) {
			// Sibling Disjoint Exception?
			if (!isException(queryObj.getNode(1), queryObj.getNode(2))) {
				queryObj.addResult(new Substitution());
				Substitution sub = siblingDisjoints.iterator().next();
				processSiblingJustification(sub.getSubstitution(transOne),
						sub.getSubstitution(transTwo),
						sub.getSubstitution(queryVar), queryObj);
				return;
			}
		} else {
			for (Substitution sub : siblingDisjoints) {
				VariableNode varNode = queryObj.getVariable();
				for (Node sibling : CommonQuery.INSTANCES.runQuery(dag_,
						sub.getSubstitution(queryVar))) {
					if (!sibling.equals(atomic)
							&& !isException(atomic, sibling))
						queryObj.addResult(new Substitution(varNode,
								(DAGNode) sibling), sub
								.getSubstitution(queryVar),
								CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
										.getNode(dag_));
				}
			}
		}
	}

	private void siblingDisjointViaModule(DAGNode atomic, QueryObject queryObj) {
		if (queryObj.isProof()) {
			DAGNode otherNode = (DAGNode) queryObj.getNode(2);

			// Find the unique parents for each set
			Collection<Node> atomicParents = CommonQuery.ALLGENLS.runQuery(
					dag_, atomic);
			Collection<Node> otherParents = CommonQuery.ALLGENLS.runQuery(dag_,
					otherNode);
			Collection<Node> commonParents = CollectionUtils.retainAll(
					atomicParents, otherParents);
			atomicParents.removeAll(commonParents);
			otherParents.removeAll(commonParents);

			// Find the sibling disjoint collections for atomics
			Collection<DAGNode> atomicSibCols = new HashSet<>();
			for (Node atomicParent : atomicParents)
				if (atomicParent instanceof DAGNode)
					atomicSibCols.addAll(sibModule_
							.getSiblingDisjointParents((DAGNode) atomicParent));

			// Search for match in others
			for (Node otherParent : otherParents) {
				if (otherParent instanceof DAGNode)
					if (CollectionUtils.containsAny(atomicSibCols, sibModule_
							.getSiblingDisjointParents((DAGNode) otherParent))) {
						// A match!
						if (!isException(atomic, otherNode)) {
							queryObj.addResult(new Substitution());
							// TODO This should probably be added back in. Can't
							// recall what the issue was - probably introduced a
							// bug
							// processSiblingJustification(,
							// sub.getSubstitution(transTwo),
							// sub.getSubstitution(queryVar), queryObj);
						}
						return;
					}
			}
		} else {
			// TODO This should be filled in.
		}
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
	 */
	private void processSiblingJustification(Node transOne, Node transTwo,
			Node queryVar, QueryObject queryObj) {
		List<Node[]> justification = queryObj.getJustification();
		QueryObject proof = null;

		// Add transitive query 1, unless it is the same
		Node arg1 = queryObj.getNode(1);
		if (!arg1.equals(transOne)) {
			proof = new QueryObject(queryObj.shouldJustify(),
					CommonConcepts.GENLS.getNode(dag_), arg1, transOne);
			querier_.prove(proof);
			if (queryObj.shouldJustify()) {
				justification.addAll(proof.getJustification());
				justification.add(new Node[0]);
			}
		}

		// Add isa sibling query for arg 1
		proof = new QueryObject(queryObj.shouldJustify(),
				CommonConcepts.ISA.getNode(dag_), transOne, queryVar);
		querier_.prove(proof);
		if (queryObj.shouldJustify()) {
			justification.addAll(proof.getJustification());
			justification.add(new Node[0]);
		}

		// Add transitive query 2, unless it is the same
		Node arg2 = queryObj.getNode(2);
		if (!arg2.equals(transTwo)) {
			proof = new QueryObject(queryObj.shouldJustify(),
					CommonConcepts.GENLS.getNode(dag_), arg2, transTwo);
			querier_.prove(proof);
			if (queryObj.shouldJustify()) {
				justification.addAll(proof.getJustification());
				justification.add(new Node[0]);
			}
		}

		// Add isa sibling query for arg 2
		proof = new QueryObject(queryObj.shouldJustify(),
				CommonConcepts.ISA.getNode(dag_), transTwo, queryVar);
		querier_.prove(proof);
		if (queryObj.shouldJustify()) {
			justification.addAll(proof.getJustification());
			justification.add(new Node[0]);
		}

		// Add isa sibling collection to sibling collection type
		proof = new QueryObject(queryObj.shouldJustify(),
				CommonConcepts.ISA.getNode(dag_), queryVar,
				CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE.getNode(dag_));
		querier_.prove(proof);
		if (queryObj.shouldJustify()) {
			justification.addAll(proof.getJustification());
		}
	}

	private boolean isException(Node node, Node node2) {
		QueryObject isException = new QueryObject(
				CommonConcepts.SIBLING_DISJOINT_EXCEPTION.getNode(dag_), node,
				node2);
		querier_.applyModule(CommonConcepts.ASSERTED_SENTENCE.getNodeName(),
				isException);
		return isException.getResults() != null;
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
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

		if (queryObj.isProof() && queryObj.getResults() != null)
			return;

		// Find sibling disjoints
		checkSiblingDisjoint(queryObj);
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

	@Override
	public void setDAG(DirectedAcyclicGraph dag) {
		super.setDAG(dag);
		sibModule_ = (SiblingDisjointModule) dag_
				.getModule(SiblingDisjointModule.class);
		disjointModule_ = (DisjointModule) dag_.getModule(DisjointModule.class);
		if (disjointModule_ == null)
			System.out.println("Warning: Disjointness calculations "
					+ "are more efficient with Disjoint Module active. "
					+ "It is recommended to restart with the Disjoint "
					+ "Module active.");
	}
}
