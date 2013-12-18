/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand
 ******************************************************************************/
package graph.module.cli;

import graph.core.DAGNode;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.DepthModule;

import java.util.Collection;

public class DepthCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{1} #D : Returns all nodes at depth #D.";
	}

	@Override
	public String shortDescription() {
		return "Returns all nodes of a certain depth (where Thing is at depth 0).";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
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

		int depth = Integer.parseInt(data);
		Collection<DAGNode> nodes = depthModule.getNodesAtDepth(depth);
		if (nodes == null) {
			print("0|");
			return;
		}

		// Apply sorting
		nodes = dagHandler.sort(nodes, rangeStart_, rangeEnd_);

		print(nodes.size() + "|");
		for (DAGNode n : nodes)
			print(dagHandler.textIDObject(n) + "|");
		print("\n");
	}

}
