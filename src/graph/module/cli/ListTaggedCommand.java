package graph.module.cli;

import java.util.Collection;

import graph.core.DAGNode;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.SubDAGExtractorModule;

public class ListTaggedCommand extends CollectionCommand {

	@Override
	public String helpText() {
		return "{0} tag : Lists all nodes tagged by <tag>.";
	}

	@Override
	public String shortDescription() {
		return "Lists all tagged nodes under a given tag.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();

		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		SubDAGExtractorModule subDAGModule = (SubDAGExtractorModule) dagHandler
				.getDAG().getModule(SubDAGExtractorModule.class);
		if (subDAGModule == null) {
			print("-1|SubDAG Extractor Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Collection<DAGNode> taggedNodes = subDAGModule.listTagged(data);
		if (taggedNodes == null) {
			print("0|\n");
			return;
		}
		dagHandler.sort(taggedNodes, rangeStart_, rangeEnd_);
		
		print(taggedNodes.size() + "|");
		for (DAGNode n : taggedNodes)
			print(dagHandler.textIDObject(n) + "|");
		print("\n");
	}
}
