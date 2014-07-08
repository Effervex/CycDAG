package graph.module.cli;

import java.util.HashSet;

import graph.core.DAGNode;
import graph.core.cli.DAGPortHandler;
import graph.module.DepthModule;
import core.Command;

public class DepthProcessTestCommand extends Command {

	@Override
	public String shortDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DepthModule depthModule = (DepthModule) dagHandler.getDAG().getModule(
				DepthModule.class);
		if (depthModule == null) {
			print("-1|Depth Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		DAGNode node = (DAGNode) dagHandler.getDAG().findOrCreateNode(data,
				null, false);
		depthModule.processNode(node, new HashSet<Integer>());
	}

}
