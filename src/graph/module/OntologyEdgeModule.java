package graph.module;

import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;

import util.Pair;

public class OntologyEdgeModule extends RelatedEdgeModule {
	private static final String FUNC_SPLIT = "F";
	private static final long serialVersionUID = -6571482554762313718L;

	@Override
	protected List<EdgeCol> locateEdgeCollections(boolean createNew,
			Object... args) {
		return locateEdgeCollections("", createNew, args);
	}

	protected List<EdgeCol> locateEdgeCollections(String functionPrefix,
			boolean createNew, Object... args) {
		List<EdgeCol> edgeCols = new ArrayList<>();
		for (int i = 0; i < args.length; i++) {
			Node n = (Node) args[i];
			boolean additive = true;

			// Get index of node
			String index = null;
			if (i < args.length - 1 && !(args[i + 1] instanceof Node)) {
				i++;
				index = args[i].toString();
				if (index.startsWith("-")) {
					additive = false;
					index = index.substring(1);
				}
			}

			// TODO This is not ideal for functions.
			String key = (index == null || functionPrefix == null) ? null
					: functionPrefix + index;
			if (n instanceof Edge) {
				if (key != null)
					key = key + FUNC_SPLIT;
				edgeCols.addAll(locateEdgeCollections(key, createNew,
						asIndexed(((Edge) n).getNodes())));
			} else if (n instanceof DAGNode) {
				Collection<Edge> edgeCol = getEdges(n, key, createNew);
				edgeCols.add(new EdgeCol(additive, edgeCol));
			}
		}
		return edgeCols;
	}

	@Override
	protected Collection<Edge> filterNonDAGs(Collection<Edge> edges,
			Object[] args) {
		Collection<Pair<Node, String>> nonDAGNodes = findNonDAGs(args);

		if (nonDAGNodes.isEmpty() || edges == null)
			return edges;

		// Check every edge (EXPENSIVE)
		Collection<Edge> filtered = new HashSet<>();
		for (Edge e : edges) {
			boolean matches = true;
			for (Pair<Node, String> nonDAG : nonDAGNodes) {
				// Locate the correct position
				Node[] currentNodes = e.getNodes();
				if (nonDAG.objB_ == null) {
					if (!ArrayUtils.contains(currentNodes, nonDAG.objA_)) {
						matches = false;
						break;
					}
					continue;
				}

				String[] split = nonDAG.objB_.split(FUNC_SPLIT);
				int i = 0;
				try {
					for (i = 0; i < split.length - 1; i++) {
						Node n = currentNodes[Integer.parseInt(split[i]) - 1];
						if (n instanceof Edge)
							currentNodes = ((Edge) n).getNodes();
						else {
							matches = false;
							break;
						}
					}
				} catch (Exception e1) {
					e1.printStackTrace();
				}

				// Check the node
				if (!currentNodes[Integer.parseInt(split[i]) - 1]
						.equals(nonDAG.objA_)) {
					matches = false;
					break;
				}
			}

			if (matches)
				filtered.add(e);
		}
		return filtered;
	}

	@Override
	protected Object[] asIndexed(Node... nodes) {
		ArrayList<Object> args = recurseIndexed(nodes, "");
		return args.toArray(new Object[args.size()]);
	}

	private ArrayList<Object> recurseIndexed(Node[] nodes, String prefix) {
		ArrayList<Object> args = new ArrayList<>(nodes.length * 2);
		for (int i = 0; i < nodes.length; i++) {
			if (nodes[i] instanceof Edge) {
				args.addAll(recurseIndexed(((Edge) nodes[i]).getNodes(), prefix
						+ (i + 1) + FUNC_SPLIT));
			} else {
				args.add(nodes[i]);
				args.add(prefix + (i + 1));
			}
		}
		return args;
	}

	/**
	 * Returns all nodes in the arguments containing nonDAG nodes (strings,
	 * numbers, etc.), along with their position.
	 * 
	 * @param args
	 *            The nodes and positions to examine.
	 * @return A collection of all non-DAG nodes, with indices.
	 */
	protected Collection<Pair<Node, String>> findNonDAGs(Object[] args) {
		Collection<Pair<Node, String>> nonDAGNodes = new ArrayList<>();
		for (int i = 0; i < args.length; i++) {
			Node node = (Node) args[i];
			String index = null;
			if (i < args.length - 1 && !(args[i + 1] instanceof Node)) {
				i++;
				index = args[i].toString();
			}

			if (node instanceof OntologyFunction) {
				Node[] subnodes = ((OntologyFunction) node).getNodes();
				Object[] subargs = new Object[subnodes.length * 2];
				for (int j = 0; j < subnodes.length; j++) {
					subargs[j * 2] = subnodes[j];
					subargs[j * 2 + 1] = index + FUNC_SPLIT + (j + 1);
				}

				Collection<Pair<Node, String>> subNonDAGs = findNonDAGs(subargs);
				if (index == null && !subNonDAGs.isEmpty())
					nonDAGNodes.add(new Pair<Node, String>(node, index));
				else
					nonDAGNodes.addAll(subNonDAGs);
			} else if (!(node instanceof DAGNode))
				nonDAGNodes.add(new Pair<Node, String>(node, index));
		}
		return nonDAGNodes;
	}
}
