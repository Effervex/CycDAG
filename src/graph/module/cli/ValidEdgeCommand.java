package graph.module.cli;

import java.util.ArrayList;

import util.UtilityMethods;
import graph.core.CycDAG;
import graph.core.DAGErrorEdge;
import graph.core.Node;
import graph.core.cli.DAGCommand;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.inference.QueryResult;

public class ValidEdgeCommand extends DAGCommand {
	@Override
	public String helpText() {
		return "{0} edgeString forceConstraint? : Checks if an edge "
				+ "has semantically valid arguments. Can optionally "
				+ "force constraints such that arguments become valid.";
	}

	@Override
	public String shortDescription() {
		return "Checks if an edge has semantically valid arguments.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		CycDAG dag = (CycDAG) dagHandler.getDAG();
		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		ArrayList<String> split = UtilityMethods.split(data, ' ');
		Node[] nodes = dag.parseNodes(split.get(0), null, false, false, false);
		if (nodes == null) {
			print("-1|Couldn't parse nodes.\n");
			return;
		}
		boolean forwardChainCreate = (split.size() == 2 && split.get(1)
				.equalsIgnoreCase("T"));

		QueryObject qo = new QueryObject(false, false, QueryResult.ALL, nodes);
		DAGErrorEdge error = dag.isSemanticallyValid(qo, null,
				forwardChainCreate, false, false);
		if (error == null)
			print("1\n");
		else
			print("0|" + error.getError(true) + "\n");
	}
}
