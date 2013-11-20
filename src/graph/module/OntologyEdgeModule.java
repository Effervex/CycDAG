package graph.module;

import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import util.Pair;

public class OntologyEdgeModule extends RelatedEdgeModule {
	private static final String FUNC_SPLIT = "F";
	private static final long serialVersionUID = -6571482554762313718L;

	private List<EdgeCol> locateEdgeCollections(String functionPrefix,
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
		return edgeNodes[index].equals(nonDAG.objA_);
	}

	@Override
	protected Object parseKeyArg(Object arg) {
		return arg.toString();
	}
}
