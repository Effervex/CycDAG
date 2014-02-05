package graph.module.cli;

import java.util.Collection;
import java.util.HashSet;
import java.util.SortedSet;

import util.collection.WeightedSet;

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.inference.CommonQuery;
import graph.module.SemanticSimilarityModule;

public class SemanticSiblingsCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} concept : Returns all siblings to concept "
				+ "ordered by their semantic similarity to the "
				+ "concept, in the format <Node>,<Similarity>.";
	}

	@Override
	public String shortDescription() {
		return "Returns all siblings of this collection "
				+ "ordered by semantic similarity.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DirectedAcyclicGraph dag = dagHandler.getDAG();
		SemanticSimilarityModule semanticModule = (SemanticSimilarityModule) dag
				.getModule(SemanticSimilarityModule.class);
		if (semanticModule == null) {
			print("-1|Semantic similarity module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Node node = dag.findDAGNode(data);

		// First, get all siblings
		Collection<Node> siblings = new HashSet<Node>(
				CommonQuery.ISASIBLINGS.runQuery(dag, node));
		siblings.addAll(CommonQuery.GENLSIBLINGS.runQuery(dag, node));

		// Then, order by semantic similarity
		WeightedSet<Node> weighted = new WeightedSet<>(siblings.size());
		for (Node n : siblings)
			weighted.add(n, semanticModule.execute(node, n));
		SortedSet<Node> ordered = weighted.getOrdered();
		print(ordered.size() + "|");
		for (Node n : ordered)
			print(dagHandler.textIDObject(n) + "," + weighted.getWeight(n)
					+ "|");
		print("\n");
	}
}
