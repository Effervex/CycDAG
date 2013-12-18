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
import graph.core.Edge;
import graph.core.Node;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.VariableNode;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

import util.collection.MultiMap;

public class DepthModule extends DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 7586206693104940128L;
	public static final String DEPTH_PROPERTY = "depth";

	private boolean depthCalculated_ = false;
	private MultiMap<Integer, DAGNode> depthMap_;
	private boolean incrementalSupported_ = false;

	public DepthModule() {
		super();
		depthMap_ = MultiMap.createConcurrentHashSetMultiMap();
	}

	private CommonConcepts determineDepthPath(Node node) {
		QueryModule querier = (QueryModule) dag_.getModule(QueryModule.class);
		// If a collection, use genls.
		QueryObject qo = new QueryObject(CommonConcepts.GENLS.getNode(dag_),
				node, new VariableNode("?X"));
		querier.applyModule(CommonConcepts.ASSERTED_SENTENCE.getNodeName(), qo);
		if (qo.getResults() != null && !qo.getResults().isEmpty())
			return CommonConcepts.GENLS;

		// If a predicate, use genlPreds, if possible
		qo = new QueryObject(CommonConcepts.GENLPREDS.getNode(dag_), node,
				new VariableNode("?X"));
		querier.applyModule(CommonConcepts.ASSERTED_SENTENCE.getNodeName(), qo);
		if (qo.getResults() != null && !qo.getResults().isEmpty())
			return CommonConcepts.GENLPREDS;

		// If a MT, use genlMt, if possible
		qo = new QueryObject(CommonConcepts.GENLMT.getNode(dag_), node,
				new VariableNode("?X"));
		querier.applyModule(CommonConcepts.ASSERTED_SENTENCE.getNodeName(), qo);
		if (qo.getResults() != null && !qo.getResults().isEmpty())
			return CommonConcepts.GENLMT;

		// Otherwise, use isa
		return CommonConcepts.ISA;
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
	private int processNode(DAGNode node, HashSet<Long> seen) {
		if (node.equals(CommonConcepts.THING.getNode(dag_))) {
			dag_.addProperty(node, DEPTH_PROPERTY, "0");
			depthMap_.put(0, node);
			return 0;
		}
		if (seen.contains(node.getID()))
			return 0;
		seen.add(node.getID());

		int depth = 1;
		String depthStr = node.getProperty(DEPTH_PROPERTY);
		if (depthStr == null || depthStr.equals("-1")) {
			Collection<Node> minCol = null;
			CommonConcepts depthType = determineDepthPath(node);
			if (depthType == CommonConcepts.GENLS)
				minCol = CommonQuery.MINGENLS.runQuery(dag_, node);
			else
				minCol = CommonQuery.MINISA.runQuery(dag_, node);
			minCol.remove(node);
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
		depthMap_.clear();
	}

	@Override
	public boolean addEdge(Edge edge) {
		if (!(edge instanceof DAGEdge) || !depthCalculated_)
			return true;

		if (!incrementalSupported_) {
			System.err.println("Incremental updates not supported "
					+ "for DepthModule!");
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
			int newDepth = processNode(subjectNode, new HashSet<Long>());

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
		int count = 0;
		int tenPercent = nodes.size() / 10;
		for (DAGNode node : nodes) {
			count++;
			processNode(node, new HashSet<Long>());
			if (count % tenPercent == 0)
				System.out.print((count / tenPercent * 10) + "% ");
		}
		System.out.println("Depth calculation complete!");
		depthCalculated_ = true;
		return true;
	}

	@Override
	public boolean removeEdge(Edge edge) {
		if (!incrementalSupported_) {
			System.err.println("Incremental updates not supported "
					+ "for DepthModule!");
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
}
