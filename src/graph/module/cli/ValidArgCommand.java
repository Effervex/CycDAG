package graph.module.cli;

import java.util.ArrayList;

import util.UtilityMethods;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import core.Command;

public class ValidArgCommand extends Command {
	@Override
	public String helpText() {
		return "{0} predicate argNum arg : Checks if an argument for an "
				+ "edge predicate is semantically valid. Note that this DOES "
				+ "NOT perform any disjointness reasoning.";
	}

	@Override
	public String shortDescription() {
		return "Checks if the provided edge arguments are semantically valid.";
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
		if (split.size() != 3) {
			print("-1|Wrong number of arguments.\n");
			return;
		}
		try {
			DAGNode predicate = (DAGNode) dagHandler.getDAG().findOrCreateNode(
					split.get(0), null, false, true, false);
			Integer argNum = Integer.parseInt(split.get(1).replaceAll("'", ""));
			Node arg = dagHandler.getDAG().findOrCreateNode(split.get(2), null,
					false, false, false);

			if (dag.singleArgCheck(predicate, argNum, arg))
				print("1\n");
			else
				print("0\n");
		} catch (Exception e) {
			print("-1|Problem parsing nodes.\n");
		}
	}

}
