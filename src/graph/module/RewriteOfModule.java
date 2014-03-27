package graph.module;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;

/**
 * Keeps track of all rewriteOf assertions and provides quick access to
 * rewritten nodes.
 * 
 * @author Sam Sarjant
 */
public class RewriteOfModule extends DAGModule<DAGNode> {
	private static final long serialVersionUID = 1L;
	private Map<DAGNode, DAGNode> rewriteMap_;

	public RewriteOfModule() {
		rewriteMap_ = new HashMap<>();
	}

	@Override
	public DAGNode execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (args == null || args.length < 1)
			return null;
		Node node = (Node) args[0];
		return rewriteMap_.get(node);
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		Node[] edgeNodes = edge.getNodes();
		if (edgeNodes[0].equals(CommonConcepts.REWRITE_OF.getNode(dag_))) {
			// Add to rewritten map
			rewriteMap_.put((DAGNode) edgeNodes[1], (DAGNode) edgeNodes[2]);

			// Go through prior edges and assert for rewritten node
		} else {
			Node[] rewrittenEdgeNodes = null;
			Node creator = null;
			boolean ephemeral = false;
			for (int i = 0; i < edgeNodes.length; i++) {
				if (rewriteMap_.containsKey(edgeNodes[i])) {
					// An edge needs rewriting
					if (rewrittenEdgeNodes == null) {
						rewrittenEdgeNodes = Arrays.copyOf(edgeNodes,
								edgeNodes.length);
						creator = dag_.findOrCreateNode(edge.getCreator(),
								null, false);
						ephemeral = edge
								.getProperty(DirectedAcyclicGraph.EPHEMERAL_MARK) != null;
					}
					rewrittenEdgeNodes[i] = rewriteMap_.get(edgeNodes[i]);
				}
			}

			if (rewrittenEdgeNodes != null) {
				// Assert rewritten edge
				dag_.findOrCreateEdge(rewrittenEdgeNodes, creator, true,
						ephemeral, true);
			}
		}
		return super.addEdge(edge);
	}
}
