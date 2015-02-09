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
package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.EdgeModifier;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.VariableNode;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

import org.apache.commons.collections4.CollectionUtils;

import util.collection.MultiMap;

public class DepthModule extends DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 7586206693104940128L;
	public static final String DEPTH_PROPERTY = "depth";

	private boolean depthCalculated_ = false;
	private MultiMap<Integer, DAGNode> depthMap_;
	private boolean incrementalSupported_ = false;

	public DepthModule() {
		super();
	}

	private Collection<Node> getMinimumParents(DAGNode node) {
		QueryModule querier = (QueryModule) dag_.getModule(QueryModule.class);

		Collection<Node> minParents = new ArrayList<>();

		// Add genls
		minParents.addAll(checkParentRelationship(node, CommonConcepts.GENLS,
				querier));

		// Add isa
		minParents.addAll(checkParentRelationship(node, CommonConcepts.ISA,
				querier));

		// Add genlPreds
		minParents.addAll(checkParentRelationship(node,
				CommonConcepts.GENLPREDS, querier));

		// Add genlMt
		minParents.addAll(checkParentRelationship(node, CommonConcepts.GENLMT,
				querier));

		// If function
		if (node instanceof OntologyFunction) {
			// Add resultIsa
			minParents.addAll(querier.functionResults((OntologyFunction) node,
					CommonConcepts.RESULT_ISA));

			// Add resultGenls
			minParents.addAll(querier.functionResults((OntologyFunction) node,
					CommonConcepts.RESULT_GENL));
		}

		minParents.remove(node);
		return minParents;// CommonQuery.minGeneralFilter(minParents, dag_);
	}

	/**
	 * Searches for all assertions with a given relationship and node placement
	 * and returns all second arg values for found assertions.
	 * 
	 * @param node
	 *            The node to search for.
	 * @param relationship
	 *            The relationship for the node.
	 * @param querier
	 *            The query module.
	 * @return The collection of all second arguments for assertions with
	 *         relationship and arg as the first and second args respectively.
	 */
	@SuppressWarnings("unchecked")
	private Collection<Node> checkParentRelationship(DAGNode node,
			CommonConcepts relationship, QueryModule querier) {
		VariableNode x = new VariableNode("?X");
		QueryObject qo = new QueryObject(relationship.getNode(dag_), node, x);
		querier.applyModule(CommonConcepts.ASSERTED_SENTENCE.getNodeName(), qo);
		Collection<Node> results = QueryModule.parseResultsFromSubstitutions(x,
				qo.getResults());
		if (results != null)
			return results;
		return CollectionUtils.EMPTY_COLLECTION;
	}

	/**
	 * Recursively calculate the node depth.
	 * 
	 * @param node
	 *            The node to calculate depth for.
	 * @param seen
	 *            If the node has been seen already in this calculation.
	 * @return The minimum depth of the above node.
	 */
	public int processNode(DAGNode node, HashSet<Integer> seen) {
		if (depthMap_ == null)
			depthMap_ = MultiMap.createConcurrentHashSetMultiMap();
		if (node.equals(CommonConcepts.THING.getNode(dag_))) {
			dag_.addProperty(node, DEPTH_PROPERTY, "0");
			depthMap_.put(0, node);
			return 0;
		}
		if (seen.contains(node.getID()))
			return 0;
		seen.add(node.getID());

		// Check for rewrite
		RewriteOfModule rewriteModule = (RewriteOfModule) dag_
				.getModule(RewriteOfModule.class);
		if (rewriteModule != null) {
			DAGNode rewrite = rewriteModule.getRewrite(node);
			if (rewrite != node) {
				processNode(rewrite, seen);
				String rewriteDepth = rewrite.getProperty(DEPTH_PROPERTY);
				int depth = Integer.parseInt(rewriteDepth);
				dag_.addProperty(node, DEPTH_PROPERTY, rewriteDepth);
				depthMap_.put(depth, node);
				return depth;
			}
		}

		int depth = 1;
		String depthStr = node.getProperty(DEPTH_PROPERTY);
		if (depthStr == null || depthStr.equals("-1")) {
			Collection<Node> minCol = getMinimumParents(node);
			for (Node superGenls : minCol) {
				depth = Math
						.max(processNode((DAGNode) superGenls, new HashSet<>(
								seen)) + 1, depth);
			}
			dag_.addProperty(node, DEPTH_PROPERTY, depth + "");
			depthMap_.put(depth, node);
		} else
			depth = Integer.parseInt(depthStr);
		return depth;
	}

	@Override
	public void clear() {
		depthCalculated_ = false;
		if (depthMap_ != null)
			depthMap_.clear();
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		if (!(edge instanceof DAGEdge) || !depthCalculated_)
			return true;

		if (!incrementalSupported_) {
			depthCalculated_ = false;
			return true;
		}

		DAGNode edgePred = (DAGNode) edge.getNodes()[0];
		if (edgePred.equals(CommonConcepts.GENLS.getNode(dag_))
				|| edgePred.equals(CommonConcepts.GENLPREDS.getNode(dag_))
				|| edgePred.equals(CommonConcepts.GENLMT.getNode(dag_))
				|| edgePred.equals(CommonConcepts.ISA.getNode(dag_))) {
			if (!(edge.getNodes()[1] instanceof DAGNode))
				return true;

			DAGNode subjectNode = (DAGNode) edge.getNodes()[1];

			String oldStr = subjectNode.getProperty(DEPTH_PROPERTY);
			int oldDepth = (oldStr == null) ? -1 : Integer.parseInt(oldStr);
			int newDepth = processNode(subjectNode, new HashSet<Integer>());

			if (newDepth != oldDepth) {
				// TODO Propagate the changes down.
			}
		}
		return true;
	}

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		// Return all nodes at depth arg
		if (args[0] instanceof Integer)
			return getNodesAtDepth((Integer) args[0]);
		return null;
	}

	public Collection<DAGNode> getNodesAtDepth(int depth) {
		return depthMap_.get(depth);
	}

	@Override
	public boolean initialisationComplete(Collection<DAGNode> nodes,
			Collection<DAGEdge> edges, boolean forceRebuild) {
		if (depthCalculated_ && !forceRebuild)
			return false;

		// Compute the depths of each node in the graph
		System.out.print("Calculating node depths... ");
		depthMap_ = MultiMap.createConcurrentHashSetMultiMap();
		int count = 0;
		int tenPercent = nodes.size() / 10;
		// TODO Embarrassingly parallel.
		for (DAGNode node : nodes) {
			count++;
			processNode(node, new HashSet<Integer>());
			if (count % tenPercent == 0)
				System.out.print((count / tenPercent * 10) + "% ");
		}
		System.out.println("Depth calculation complete!");
		depthCalculated_ = true;
		return true;
	}

	@Override
	public boolean removeEdge(DAGEdge edge) {
		if (!incrementalSupported_) {
			depthCalculated_ = false;
			return true;
		}

		return true;
	}

	@Override
	public Collection<String> getPertinentProperties() {
		Collection<String> props = new ArrayList<String>(1);
		props.add(DEPTH_PROPERTY);
		return props;
	}

	@Override
	public void disableCached() {
		depthCalculated_ = false;
		depthMap_ = null;
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return !EdgeModifier.isSpecial(edge, dag_);
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}
}
