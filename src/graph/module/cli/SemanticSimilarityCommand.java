package graph.module.cli;

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.module.SemanticSimilarityModule;
import core.Command;

public class SemanticSimilarityCommand extends Command {
	@Override
	public String helpText() {
		return "{0} concept1 concept2 : Calculates the similarity "
				+ "between concept1 and concept2, where a similarity "
				+ "of 1 = identical, and similarity 0 = completely "
				+ "disconnected.";
	}

	@Override
	public String shortDescription() {
		return "Calculates the semantic similarity between two concepts.";
	}

	@Override
	protected void executeImpl() {
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

		Node[] nodes = dag.parseNodes(data, null, false, true);
		if (nodes.length != 2) {
			print("-1|Requires two node arguments.\n");
			return;
		}

		float similarity = semanticModule.execute(nodes[0], nodes[1]);
		print(similarity + "\n");
	}
}
