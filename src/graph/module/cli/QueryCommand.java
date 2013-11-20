package graph.module.cli;

import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import core.Command;

public class QueryCommand extends Command {
	private static final Pattern ARG_PATTERN = Pattern
			.compile("^(\\(.+?\\))( \\[(\\d+), *(\\d+)\\))?$");

	@Override
	public String helpText() {
		return "{0} (X Y ...) [A,B) : Poses a query to the DAG "
				+ "in the form of a bracketed edge expression "
				+ "consisting of either nodes (ID or named) "
				+ "or variables (in the form ?X or ? for "
				+ "'don't care' variables).\nThe query returns "
				+ "the number of valid substitutions (0 if there "
				+ "are none), followed by the variable substitutions."
				+ "\nUse A and B to define a subset of results "
				+ "returned [incl-excl).";
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
			print("-1|Query module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Matcher m = ARG_PATTERN.matcher(data.trim());
		if (!m.matches()) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		Node[] args = null;
		args = dagHandler.getDAG().parseNodes(m.group(1), null, false, true);
		if (args == null) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		// Subset args
		int start = 0;
		int end = Integer.MAX_VALUE;
		if (m.group(2) != null) {
			start = Integer.parseInt(m.group(3));
			end = Integer.parseInt(m.group(4));
			if (end <= start) {
				print("-2|Invalid range argument.\n");
				return;
			}
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

			int size = substitutions.size();
			size = Math.min(size, Math.min(size, end) - start);
			print(size + "|");
		}

		Substitution[] subArray = substitutions
				.toArray(new Substitution[substitutions.size()]);
		for (int i = start; i < subArray.length && i < end; i++) {
			Map<String, Node> varSub = subArray[i].getSubstitutionMap();
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
