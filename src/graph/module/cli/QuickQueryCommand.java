package graph.module.cli;

import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.CommonQuery;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import util.UtilityMethods;
import core.Command;

public class QuickQueryCommand extends Command {
	private static final Pattern ARG_PATTERN = Pattern
			.compile("^(\\S+)(.+?)( \\[(\\d+), *(\\d+)\\))?$");

	@Override
	public String helpText() {
		StringBuffer buffer = new StringBuffer(
				"{0} <shortCommand> <nodeArgs> [A,B) : Poses a query "
						+ "to the DAG using the CommonQuery enum. "
						+ "Each query returns a set of nodes. Use A "
						+ "and B to define a subset of results "
						+ "returned [incl-excl).\nQueries include: ");
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

		Matcher m = ARG_PATTERN.matcher(data.trim());
		if (!m.matches()) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		CommonQuery cq;
		try {
			cq = CommonQuery.valueOf(m.group(1).toUpperCase());
		} catch (IllegalArgumentException e) {
			print("-1|Quick Query not found.\n");
			return;
		}

		int firstSpace = data.indexOf(' ');
		if (firstSpace == -1) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		ArrayList<String> split = UtilityMethods.split(m.group(2).trim(), ' ');
		Node[] args = new Node[split.size()];
		for (int i = 0; i < args.length; i++) {
			args[i] = dagHandler.getDAG().findOrCreateNode(split.get(i), null,
					false, false, false);
			if (args[i] == null) {
				print("-1|Could not parse arguments.\n");
				return;
			}
		}

		// Subset args
		int start = 0;
		int end = Integer.MAX_VALUE;
		if (m.group(3) != null) {
			start = Integer.parseInt(m.group(4));
			end = Integer.parseInt(m.group(5));
			if (end <= start) {
				print("-2|Invalid range argument.\n");
				return;
			}
		}

		Collection<Node> result = cq.runQuery(dagHandler.getDAG(), args);
		if (result == null || result.isEmpty()) {
			print("0|NIL\n");
			return;
		} else {
			int size = result.size();
			size = Math.min(size, Math.min(size, end) - start);
			print(size + "|");
		}

		Node[] resultArray = result.toArray(new Node[result.size()]);
		for (int i = start; i < resultArray.length && i < end; i++) {
			if (resultArray[i] != null)
				print(dagHandler.textIDObject(resultArray[i]));
			print("|");
		}
		print("\n");
	}

}
