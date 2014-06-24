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

import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.apache.commons.lang3.StringUtils;

import util.Pair;
import util.collection.MultiMap;

public class OntologyEdgeModule extends RelatedEdgeModule {
	private static final String FUNC_SPLIT = "F";
	private static final long serialVersionUID = -6571482554762313718L;

	/**
	 * Gets the function edges (or edges containing the node NOT in functions).
	 * 
	 * @param n
	 *            The node to get edges for.
	 * @param functionOnly
	 *            If only getting edges in which the node is in the function, or
	 *            getting non-function edges.
	 * @return The set of all edges (function or not).
	 */
	private Collection<Edge> getFunctionEdges(Node n, boolean functionOnly) {
		Collection<Edge> funcEdges = new HashSet<>();
		MultiMap<Object, Edge> nodeEdges = relatedEdges_.get(n);
		if (nodeEdges == null)
			return funcEdges;
		for (Object key : nodeEdges.keySet()) {
			if (functionOnly && key.toString().contains(FUNC_SPLIT))
				funcEdges.addAll(nodeEdges.get(key));
			else if (!functionOnly && !key.toString().contains(FUNC_SPLIT))
				funcEdges.addAll(nodeEdges.get(key));
		}
		return funcEdges;
	}

	private List<EdgeCol> locateEdgeCollections(String functionPrefix,
			boolean createNew, Object... args) {
		List<EdgeCol> edgeCols = new ArrayList<>();
		Node predicate = null;
		for (int i = 0; i < args.length; i++) {
			Node n = (Node) args[i];
			boolean additive = true;
			boolean allBut = (functionPrefix != null && functionPrefix
					.startsWith("!"));

			// Get index of node
			String index = null;
			if (i < args.length - 1 && !(args[i + 1] instanceof Node)) {
				i++;
				index = args[i].toString();
				if (index.startsWith("-")) {
					additive = false;
					index = index.substring(1);
				} else if (index.startsWith("!")) {
					allBut = true;
					index = index.substring(1);
				} else if (index.equals("1"))
					predicate = n;
			}

			// Only function as index
			if (index != null && index.equals(FUNC_SPLIT)) {
				edgeCols.add(new EdgeCol(true, getFunctionEdges(n, additive)));
				continue;
			}

			// TODO This is not ideal for functions.
			String key = (index == null || functionPrefix == null) ? null
					: functionPrefix + index;
			if (allBut && key != null && !key.startsWith("!"))
				key = "!" + key;
			if (n instanceof Edge) {
				if (key != null)
					key = key + FUNC_SPLIT;
				edgeCols.addAll(locateEdgeCollections(key, createNew,
						asIndexed(((Edge) n).getNodes())));
			} else if (n instanceof DAGNode) {
				Collection<Edge> edgeCol = (allBut) ? getAllButEdges(n, key)
						: getEdges(n, key, createNew);
				edgeCols.add(new EdgeCol(additive, edgeCol));
			} else if (canSearchHashedModule(predicate, n)) {
				Collection<Edge> edgeCol = stringHashedModule_.execute(n
						.getName());
				edgeCols.add(new EdgeCol(additive, edgeCol));
			}
		}
		return edgeCols;
	}

	private ArrayList<Object> recurseIndexed(Node[] nodes, String prefix) {
		ArrayList<Object> args = new ArrayList<>(nodes.length * 2);
		for (int i = 0; i < nodes.length; i++) {
			if (nodes[i] instanceof Edge) {
				args.addAll(recurseIndexed(((Edge) nodes[i]).getNodes(), prefix
						+ (i + 1) + FUNC_SPLIT));
			} else {
				args.add(nodes[i]);
				args.add((prefix + (i + 1)).intern());
			}
		}
		return args;
	}

	@Override
	protected void addIfNonDAG(Node node, Object key,
			Collection<Pair<Node, Object>> nonDAGNodes) {
		if (node instanceof OntologyFunction) {
			Node[] subnodes = ((OntologyFunction) node).getNodes();
			Object[] subargs = new Object[subnodes.length * 2];
			for (int j = 0; j < subnodes.length; j++) {
				subargs[j * 2] = subnodes[j];
				subargs[j * 2 + 1] = key + FUNC_SPLIT + (j + 1);
			}

			Collection<Pair<Node, Object>> subNonDAGs = findNonDAGs(subargs);
			if (key == null && !subNonDAGs.isEmpty())
				nonDAGNodes.add(new Pair<Node, Object>(node, key));
			else
				nonDAGNodes.addAll(subNonDAGs);
		} else if (!(node instanceof DAGNode))
			nonDAGNodes.add(new Pair<Node, Object>(node, key));
	}

	@Override
	protected Object[] asIndexed(Node... nodes) {
		ArrayList<Object> args = recurseIndexed(nodes, "");
		return args.toArray(new Object[args.size()]);
	}

	@Override
	protected Object defaultKey() {
		return null;
	}

	@Override
	protected List<EdgeCol> locateEdgeCollections(boolean createNew,
			Object... args) {
		return locateEdgeCollections("", createNew, args);
	}

	@Override
	protected boolean matchingNonDAG(Pair<Node, Object> nonDAG, Node[] edgeNodes) {
		String[] split = ((String) nonDAG.objB_).split(FUNC_SPLIT);
		int i = 0;
		for (i = 0; i < split.length - 1; i++) {
			int index = Integer.parseInt(split[i]) - 1;
			if (index >= edgeNodes.length)
				return false;

			Node n = edgeNodes[index];
			if (n instanceof Edge)
				edgeNodes = ((Edge) n).getNodes();
			else
				return false;
		}

		int index = Integer.parseInt(split[i]) - 1;
		if (index >= edgeNodes.length)
			return false;

		// Check the node
		return edgeNodes[index].toString().equals(nonDAG.objA_.toString());
	}

	@Override
	protected Object parseKeyArg(Object arg) {
		return arg.toString();
	}

	/**
	 * Gets all edges but the one given by the key.
	 * 
	 * @param node
	 *            The edges must include this node.
	 * @param butEdgeKey
	 *            The key that is NOT added to the results.
	 * @return A collection of edges that are indexed by node, but none from the
	 *         butEdgeKey (though they may be added if included under other
	 *         keys).
	 */
	public Collection<Edge> getAllButEdges(Node node, Object butEdgeKey) {
		MultiMap<Object, Edge> indexedEdges = relatedEdges_.get(node);
		if (indexedEdges == null) {
			return new ConcurrentLinkedQueue<>();
		}

		Collection<Edge> edges = new HashSet<>();
		for (Object key : indexedEdges.keySet()) {
			if (!key.equals(butEdgeKey)) {
				// Need to check same function level as butEdge
				if (StringUtils.countMatches((String) key, FUNC_SPLIT) == StringUtils
						.countMatches((String) butEdgeKey, FUNC_SPLIT))
					edges.addAll(indexedEdges.get(key));
			}
		}
		return edges;
	}
}
