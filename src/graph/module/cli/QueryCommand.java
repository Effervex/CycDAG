package graph.module.cli;

import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;
import java.util.Map;

import core.Command;

public class QueryCommand extends Command {
	@Override
	public String helpText() {
		return "{0} (X Y ...) : Poses a query to the DAG "
				+ "in the form of a bracketed edge expression "
				+ "consisting of either nodes (ID or named) "
				+ "or variables (in the form ?X or ? for "
				+ "'don't care' variables).\nThe query returns "
				+ "the number of valid substitutions (0 if there "
				+ "are none), followed by the variable substitutions.";
	}

	@Override
	public String shortDescription() {
		return "Poses a query to the DAG which returns a variable substitution map.";
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
			if (qo.isProof()) {
				print("1|T|\n");
				return;
			}
			print("" + substitutions.size() + "|");
		}
		for (Substitution substitution : substitutions) {
			Map<String, Node> varSub = substitution.getSubstitutionMap();
			boolean first = true;
			for (String var : varSub.keySet()) {
				if (!first)
					print(",");
				print(var + "/" + dagHandler.textIDObject(varSub.get(var)));
				first = false;
			}
			print("|");
		}
		print("\n");
	}
}
