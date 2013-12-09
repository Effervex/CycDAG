package graph.module.cli;

import java.util.HashMap;
import java.util.LinkedList;

import graph.core.DAGNode;
import graph.core.cli.DAGPortHandler;
import graph.module.TransitiveIntervalSchemaModule;
import core.Command;

public class CyclicCheckCommand extends Command {

	@Override
	public String shortDescription() {
		return "A temporary command for cycle checking.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		TransitiveIntervalSchemaModule transitiveModule = (TransitiveIntervalSchemaModule) dagHandler
				.getDAG().getModule(TransitiveIntervalSchemaModule.class);
		if (transitiveModule == null) {
			print("-1|Transitive Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		DAGNode arg = (DAGNode) dagHandler.getDAG().findOrCreateNode(data,
				null, false, true, false);
		if (arg != null) {
			LinkedList<DAGNode> sortedNodes = new LinkedList<>();
			transitiveModule.topologicalVisit((DAGNode) arg, sortedNodes,
					new HashMap<DAGNode, String>());
//			if (result != null) {
//				print("1|" + result + "\n");
//			} else
//				print("0|\n");
		} else
			print("-1|Could not parse node.\n");
	}
}