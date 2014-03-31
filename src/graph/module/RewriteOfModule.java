package graph.module;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
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
	private transient RelatedEdgeModule relEdgeModule_;

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
		if (relEdgeModule_ == null)
			relEdgeModule_ = (RelatedEdgeModule) dag_
					.getModule(RelatedEdgeModule.class);
		Node[] edgeNodes = edge.getNodes();
		
		if (edgeNodes[0].equals(CommonConcepts.REWRITE_OF.getNode(dag_))) {
			// Add to rewritten map
			rewriteMap_.put((DAGNode) edgeNodes[2], (DAGNode) edgeNodes[1]);

			// Go through prior edges and assert for rewritten node
			Collection<Edge> associatedEdges = relEdgeModule_
					.execute(edgeNodes[2]);
			Node creator = null;
			if (edge.getCreator() != null)
				creator = dag_.findOrCreateNode(edge.getCreator(), null,
						false);
			boolean ephemeral = edge
					.getProperty(DirectedAcyclicGraph.EPHEMERAL_MARK) != null;
			
			// Propagate all edges for the rewritten node forward to the target.
			for (Edge e : associatedEdges) {
				if (e.getNodes()[0].equals(CommonConcepts.REWRITE_OF
						.getNode(dag_)))
					continue;
				// Alter the edge nodes
				Node[] rewrittenEdgeNodes = Arrays.copyOf(e.getNodes(),
						e.getNodes().length);
				for (int i = 0; i < rewrittenEdgeNodes.length; i++)
					if (rewrittenEdgeNodes[i].equals(edgeNodes[2]))
						rewrittenEdgeNodes[i] = edgeNodes[1];
				dag_.findOrCreateEdge(rewrittenEdgeNodes, creator, true,
						ephemeral, true);
				
				// Remove the associated edge
				dag_.removeEdge(e);
			}
		} else {
			Node[] rewrittenEdgeNodes = null;
			for (int i = 0; i < edgeNodes.length; i++) {
				if (rewriteMap_.containsKey(edgeNodes[i])) {
					// An edge needs rewriting
					if (rewrittenEdgeNodes == null) {
						rewrittenEdgeNodes = Arrays.copyOf(edgeNodes,
								edgeNodes.length);
					}
					rewrittenEdgeNodes[i] = rewriteMap_.get(edgeNodes[i]);
				}
			}
			
			Node creator = null;
			if (edge.getCreator() != null)
				creator = dag_.findOrCreateNode(edge.getCreator(), null,
						false);
			boolean ephemeral = edge
					.getProperty(DirectedAcyclicGraph.EPHEMERAL_MARK) != null;

			if (rewrittenEdgeNodes != null) {
				// Assert rewritten edge
				dag_.findOrCreateEdge(rewrittenEdgeNodes, creator, true,
						ephemeral, true);
				// Do not accept the edge
				return false;
			}
		}
		return true;
	}
}
