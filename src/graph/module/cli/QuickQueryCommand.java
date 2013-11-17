package graph.module.cli;

import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.CommonQuery;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;

import util.UtilityMethods;
import core.Command;

public class QuickQueryCommand extends Command {
	@Override
	public String helpText() {
		StringBuffer buffer = new StringBuffer(
				"{0} shortCommand nodeArgs : Poses a query to the DAG using the "
						+ "CommonQuery enum. Each query returns a set of nodes. "
						+ "Queries include: ");
		boolean first = true;
		for (CommonQuery cq : CommonQuery.values()) {
			if (!first)
				buffer.append(", ");
			buffer.append(cq);
			first = false;
		}
		buffer.append(". See java file for full details.");
		return buffer.toString();
	}

	@Override
	public String shortDescription() {
		return "Poses a query to the dag using short syntax.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		QueryModule queryModule = (QueryModule) dagHandler.getDAG().getModule(
				QueryModule.class);
		if (queryModule == null) {
			print("-1|Query module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		ArrayList<String> split = UtilityMethods.split(data, ' ');
		CommonQuery cq;
		try {
			cq = CommonQuery.valueOf(split.get(0).toUpperCase());
		} catch (IllegalArgumentException e) {
			print("-1|Quick Query not found.\n");
			return;
		}

		int firstSpace = data.indexOf(' ');
		if (firstSpace == -1) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		Node[] args = new Node[split.size() - 1];
		for (int i = 0; i < args.length; i++) {
			args[i] = dagHandler.getDAG().findOrCreateNode(split.get(i + 1),
					null, false, false, false);
			if (args[i] == null) {
				print("-1|Could not parse arguments.\n");
				return;
			}
		}

		Collection<Node> result = cq.runQuery(dagHandler.getDAG(), args);
		if (result == null || result.isEmpty()) {
			print("0|NIL\n");
			return;
		} else
			print(result.size() + "|");
		for (Node n : result) {
			if (n != null)
				print(dagHandler.textIDObject(n));
			print("|");
		}
		print("\n");
	}

}
