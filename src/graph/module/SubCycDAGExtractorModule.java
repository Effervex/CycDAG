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
package graph.module;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.CommonQuery;
import graph.inference.Substitution;
import graph.inference.VariableNode;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;

public class SubCycDAGExtractorModule extends SubDAGExtractorModule {
	private static final long serialVersionUID = 1L;
	private QueryModule querier_;

	@Override
	public void findLinks(Collection<DAGNode> nodes,
			Collection<DAGNode> existingNodes, int distance,
			RelatedEdgeModule relatedEdgeModule) {
		Collection<Node> linkedNodes = new HashSet<>();
		boolean allowIndividuals = true;
		for (Node n : nodes) {
			// Follow the nodes all the way up
			linkedNodes.addAll(CommonQuery.ALLISA.runQuery(dag_, n));
			linkedNodes.addAll(CommonQuery.ALLGENLS.runQuery(dag_, n));
			linkedNodes.addAll(CommonQuery.GENLPREDS.runQuery(dag_, n));

			// Follow the nodes all the way down (if distance is positive)
			if (distance > 0) {
				if (allowIndividuals)
					linkedNodes.addAll(CommonQuery.INSTANCES.runQuery(dag_, n));
				linkedNodes.addAll(CommonQuery.SPECS.runQuery(dag_, n));
				linkedNodes.addAll(CommonQuery.SPECPREDS.runQuery(dag_, n));
			}
		}

		for (Node n : linkedNodes)
			processNode(n, nodes, existingNodes);
	}

	private void processNode(Node n, Collection<DAGNode> nodes,
			Collection<DAGNode> existingNodes) {
		if (n instanceof DAGNode && !nodes.contains(n)
				&& !existingNodes.contains(n)) {
			nodes.add((DAGNode) n);
			processFunction(n, nodes, existingNodes);
			processPredicate(n, nodes, existingNodes);
		}
	}

	@Override
	protected DirectedAcyclicGraph createNewDAG(File folder) {
		CycDAG dag = new CycDAG(folder);
		dag.loadAssertions_ = false;
		querier_ = (QueryModule) dag_.getModule(QueryModule.class);
		return dag;
	}

	@Override
	public boolean growSubDAG(String tag, int distance) {
		querier_ = (QueryModule) dag_.getModule(QueryModule.class);
		return super.growSubDAG(tag, distance);
	}

	/**
	 * Processes a predicate node, adding its semantic arg constraints to the
	 * subDAG.
	 * 
	 * @param node
	 *            The node to process.
	 * @param nodes
	 *            The set of nodes to add to.
	 * @param existingNodes
	 *            Existing nodes already integrated.
	 */
	private void processPredicate(Node node, Collection<DAGNode> nodes,
			Collection<DAGNode> existingNodes) {
		VariableNode y = new VariableNode("?Y");
		VariableNode x = new VariableNode("?X");
		Collection<Substitution> subs = querier_.execute(
				CommonConcepts.ARGISA.getNode(dag_), node, x, y);
		subs.addAll(querier_.execute(CommonConcepts.ARGGENL.getNode(dag_),
				node, x, y));
		subs.addAll(querier_.execute(CommonConcepts.RESULT_ISA.getNode(dag_),
				node, y));
		subs.addAll(querier_.execute(CommonConcepts.RESULT_GENL.getNode(dag_),
				node, y));
		for (Substitution sub : subs) {
			Node constraint = sub.getSubstitution(y);
			if (constraint instanceof DAGNode)
				processNode(constraint, nodes, existingNodes);
		}
	}

	/**
	 * Processes a NART/NAT, adding its components to the subDAG.
	 * 
	 * @param node
	 *            The node to process.
	 * @param nodes
	 *            The set of nodes to add to.
	 * @param existingNodes
	 *            Existing nodes already integrated.
	 */
	private void processFunction(Node node, Collection<DAGNode> nodes,
			Collection<DAGNode> existingNodes) {
		if (node instanceof OntologyFunction) {
			Node[] funcNodes = ((OntologyFunction) node).getNodes();
			for (Node n : funcNodes)
				processNode(n, nodes, existingNodes);
		}
	}
}
