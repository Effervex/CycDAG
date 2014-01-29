package graph.module.cli;

import java.util.Collection;
import java.util.HashSet;

import graph.core.Edge;
import graph.core.Node;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.RelatedEdgeModule;

public class PredsForCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} concept : Returns all predicates in which concept is used in an assertion.";
	}

	@Override
	public String shortDescription() {
		return "Returns all predicates in which the concept is an argument for.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		RelatedEdgeModule relatedEdge = (RelatedEdgeModule) dagHandler.getDAG()
				.getModule(RelatedEdgeModule.class);
		if (relatedEdge == null) {
			print("-1|Related Edge Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Node conceptNode = dagHandler.getDAG().findOrCreateNode(data, null);
		if (conceptNode == null) {
			print("-1|Could not parse node.\n");
			return;
		}

		Collection<Node> preds = new HashSet<>();
		Collection<Edge> edges = relatedEdge.getAllButEdges(conceptNode, "1");
		for (Edge e : edges)
			preds.add(e.getNodes()[0]);
		
		preds = dagHandler.sort(preds, rangeStart_, rangeEnd_);
		
		print(preds.size() + "|");
		for (Node pred : preds)
			print(dagHandler.textIDObject(pred) + "|");
		print("\n");
	}
}
