package graph.module.cli;

import java.util.ArrayList;
import java.util.Collection;

import util.UtilityMethods;
import graph.core.DAGNode;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.TransitiveIntervalSchemaModule;

public class TransitiveCommand extends CollectionCommand {

	@Override
	public String shortDescription() {
		return "A temporary command for testing transitive intervals.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
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

		ArrayList<String> nodes = UtilityMethods.split(data, ' ');
		boolean upwards = nodes.get(0).toLowerCase().equals("u");
		DAGNode nodeA = null;
		DAGNode nodeB = null;
		try {
			nodeA = (DAGNode) dagHandler.getDAG().findOrCreateNode(
					nodes.get(1), null, false, true, false);
			if (nodes.size() >= 3)
				nodeB = (DAGNode) dagHandler.getDAG().findOrCreateNode(
						nodes.get(2), null, false, true, false);
		} catch (Exception e) {
			print("-1|Problem parsing node(s).\n");
			return;
		}

		Collection<DAGNode> result = null;
		if (nodeB == null)
			result = transitiveModule.execute(upwards, nodeA);
		else
			result = transitiveModule.execute(upwards, nodeA, nodeB);
		if (result == null)
			print("0");
		else {
			if (nodeB != null)
				print("1");
			else {
				result = dagHandler.sort(result, rangeStart_, rangeEnd_);
				print(result.size() + "|");
				for (DAGNode n : result)
					print(dagHandler.textIDObject(n) + "|");
			}
		}
		print("\n");
	}
}
