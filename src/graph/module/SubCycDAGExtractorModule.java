package graph.module;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.inference.CommonQuery;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;

public class SubCycDAGExtractorModule extends SubDAGExtractorModule {
	private static final long serialVersionUID = 1L;

	@Override
	public void followEdges(Collection<DAGNode> nodes, int distance,
			RelatedEdgeModule relatedEdgeModule) {
		// Similar process, but define the ontology as connected to an upper
		// node (Thing)
		Collection<Node> linkedNodes = new HashSet<>();
		boolean allowIndividuals = true;

		// Retrieve ALL upwards and downwards nodes & links from tagged nodes
		for (Node n : nodes) {
			linkedNodes.addAll(CommonQuery.ALLISA.runQuery(dag_, n));
			linkedNodes.addAll(CommonQuery.ALLGENLS.runQuery(dag_, n));
			if (allowIndividuals)
				linkedNodes.addAll(CommonQuery.INSTANCES.runQuery(dag_, n));
			linkedNodes.addAll(CommonQuery.SPECS.runQuery(dag_, n));
		}

		// Follow non-taxonomic nodes as normal (but only upwards searches
		// allowed)
		Collection<DAGNode> completed = new HashSet<>();
		Collection<DAGNode> currentLevel = nodes;
		for (int d = distance; d > 0; d--) {
			Collection<DAGNode> nextLevel = new HashSet<>();
			// For every node on this level
			for (DAGNode n : currentLevel) {
				// Follow non taxonomic edges
				Collection<Edge> relatedEdges = relatedEdgeModule.execute(n,
						-1, n);
				for (Edge e : relatedEdges) {
					// Non-taxonomic only!
					if (!e.equals(CommonConcepts.ISA.getNode(dag_))
							&& !e.equals(CommonConcepts.GENLS.getNode(dag_))) {
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

		// Follow taxonomic edges up
		for (Node n : linkedNodes) {
			if (!(n instanceof DAGNode))
				continue;
			nodes.add((DAGNode) n);
			Collection<Node> parents = CommonQuery.ALLISA.runQuery(dag_, n);
			parents.addAll(CommonQuery.ALLGENLS.runQuery(dag_, n));
			for (Node parent : parents)
				if (parent instanceof DAGNode)
					nodes.add((DAGNode) parent);
		}
	}

	@Override
	protected DirectedAcyclicGraph createNewDAG(File folder) {
		CycDAG dag = new CycDAG(folder);
		return dag;
	}
}
