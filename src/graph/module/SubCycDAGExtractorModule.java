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
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
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
	public void followEdges(Collection<DAGNode> nodes, int distance,
			RelatedEdgeModule relatedEdgeModule) {
		// Similar process, but define the ontology as connected to an upper
		// node (Thing)
		Collection<Node> linkedNodes = new HashSet<>();
		boolean allowIndividuals = true;

		// Retrieve ALL upwards and downwards nodes & links from tagged nodes
		for (Node n : nodes) {
			if (allowIndividuals)
				linkedNodes.addAll(CommonQuery.INSTANCES.runQuery(dag_, n));
			linkedNodes.addAll(CommonQuery.SPECS.runQuery(dag_, n));
			linkedNodes.addAll(CommonQuery.SPECPREDS.runQuery(dag_, n));
		}

		// Follow non-taxonomic nodes as normal
		Collection<DAGNode> completed = new HashSet<>();
		Collection<DAGNode> currentLevel = nodes;
		for (int d = distance; d > 0; d--) {
			Collection<DAGNode> nextLevel = new HashSet<>();
			// For every node on this level
			for (DAGNode n : currentLevel) {
				// Follow non taxonomic edges
				Collection<Edge> relatedEdges = relatedEdgeModule.execute(n);
				for (Edge e : relatedEdges) {
					// Non-taxonomic only!
					if (!e.equals(CommonConcepts.ISA.getNode(dag_))
							&& !e.equals(CommonConcepts.GENLS.getNode(dag_))
							&& !e.equals(CommonConcepts.GENLPREDS.getNode(dag_))) {
						// Grab every non-predicate argument
						Node[] args = e.getNodes();
						for (int i = 1; i < args.length; i++) {
							// Add to next level if not already looked at
							if (args[i] instanceof DAGNode
									&& !completed.contains(args[i]))
								nextLevel.add((DAGNode) args[i]);
						}
					}
				}
			}
			currentLevel = nextLevel;
			// Add all the next level nodes
			linkedNodes.addAll(nextLevel);
		}

		for (Node n : linkedNodes)
			nodes.add((DAGNode) n);
	}

	@Override
	protected DirectedAcyclicGraph createNewDAG(File folder) {
		CycDAG dag = new CycDAG(folder);
		dag.loadAssertions_ = false;
		querier_ = (QueryModule) dag_.getModule(QueryModule.class);
		return dag;
	}

	@Override
	protected void preAssertionProcessing(Collection<DAGNode> newlyAddedNodes,
			Collection<DAGNode> processedNodes, Collection<DAGEdge> edges) {
		Collection<DAGNode> parentNodes = new HashSet<>();
		for (Node node : newlyAddedNodes)
			processNode(node, parentNodes, processedNodes);
		newlyAddedNodes.addAll(parentNodes);

		super.preAssertionProcessing(newlyAddedNodes, processedNodes, edges);
	}

	private void processNode(Node node, Collection<DAGNode> parentNodes,
			Collection<DAGNode> processedNodes) {
		// Follow node upwards
		if (!(node instanceof DAGNode))
			return;
		Collection<Node> parents = CommonQuery.ALLISA.runQuery(dag_, node);
		parents.addAll(CommonQuery.ALLGENLS.runQuery(dag_, node));
		parents.addAll(CommonQuery.GENLPREDS.runQuery(dag_, node));
		for (Node parent : parents) {
			if (parent instanceof DAGNode) {
				if (!processedNodes.contains(parent)
						&& parentNodes.add((DAGNode) parent)) {
					processFunction(parent, parentNodes, processedNodes);
					processPredicate(parent, parentNodes, processedNodes);
				}
			}
		}

		if (!processedNodes.contains(node) && parentNodes.add((DAGNode) node)) {
			// Add function components
			processFunction(node, parentNodes, processedNodes);

			// Add arg constraints for predicates and functions
			processPredicate(node, parentNodes, processedNodes);
		}
	}

	private void processPredicate(Node node, Collection<DAGNode> parentNodes,
			Collection<DAGNode> processedNodes) {
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
			if (constraint instanceof DAGNode
					&& parentNodes.add((DAGNode) constraint))
				processNode(constraint, parentNodes, processedNodes);
		}
	}

	private void processFunction(Node node, Collection<DAGNode> parentNodes,
			Collection<DAGNode> processedNodes) {
		if (node instanceof OntologyFunction) {
			Node[] funcNodes = ((OntologyFunction) node).getNodes();
			for (Node n : funcNodes)
				processNode(n, parentNodes, processedNodes);
		}
	}
}
