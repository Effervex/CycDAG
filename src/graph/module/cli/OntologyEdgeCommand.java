package graph.module.cli;

import graph.core.Node;
import graph.core.cli.DAGPortHandler;

import java.util.ArrayList;

import util.UtilityMethods;

public class OntologyEdgeCommand extends RelatedEdgeCommand {
	@Override
	public String helpText() {
		return "{0} node [(nodePosition)] {1,} : "
				+ "Returns all edges using the provided node(s), "
				+ "each optionally bounded to a specific argument "
				+ "position in the edge's arguments (specified "
				+ "with optional function nesting as \\dF\\d...). "
				+ "Can also exclude/explicitly include "
				+ "function-based nodes with arg '-F' or "
				+ "'F' respectively.";
	}

	@Override
	protected Object[] parseArgs(String data, DAGPortHandler dagHandler) {
		ArrayList<Object> args = new ArrayList<>();
		int i = 0;
		ArrayList<String> split = UtilityMethods.split(data, ' ');
		while (i < split.size()) {
			Node node = dagHandler.getDAG().findOrCreateNode(split.get(i++),
					null, false, false, false);
			if (node == null) {
				print("-1|No node found.\n");
				return null;
			}
			args.add(node);

			if (i < split.size() && split.get(i).matches("\\([-\\dF]+\\)")) {
				String argPos = UtilityMethods.shrinkString(split.get(i++), 1);
				args.add(argPos);
			}
		}
		return args.toArray(new Object[args.size()]);
	}
}
