package graph.module.cli;

import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;
import java.util.List;

import core.Command;

public class JustifyCommand extends Command {
	@Override
	public String helpText() {
		return "{0} (X Y ...) : Poses a query to the DAG "
				+ "in the form of a bracketed edge expression "
				+ "consisting of nodes (ID or named).\n"
				+ "If true, a justification of why it is true "
				+ "based on known edges is returned, "
				+ "or NIL if it is false.";
	}

	@Override
	public String shortDescription() {
		return "Returns the justification for why a query is true.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		QueryModule queryModule = (QueryModule) dagHandler.getDAG().getModule(
				QueryModule.class);
		if (queryModule == null) {
			print("Query module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Node[] args = null;
		try {
			args = dagHandler.getDAG().parseNodes(data, null, false, true);
		} catch (Exception e) {
		}

		if (args == null) {
			print("-1|Could not parse arguments.\n");
			return;
		}
		QueryObject qo = new QueryObject(args);
		Collection<Substitution> substitutions = queryModule.execute(qo);
		if (substitutions == null || substitutions.isEmpty()) {
			print("0|NIL\n");
			return;
		} else {
			List<Node[]> justification = qo.getJustification();
			print(justification.size() + "|");
			for (Node[] edge : justification) {
				boolean first = true;
				print("(");
				for (Node n : edge) {
					if (!first)
						print(" ");
					print(dagHandler.textIDObject(n));
					first = false;
				}
				print(")|");
			}
			print("\n");
		}
	}

}
