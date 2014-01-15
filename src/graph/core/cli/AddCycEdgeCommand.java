package graph.core.cli;

import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AddCycEdgeCommand extends AddEdgeCommand {
	private static final Pattern ARG_PATTERN = Pattern
			.compile("^(\\(.+?\\))(?::(\\S+))?(?: \\((.+?)\\))?$");

	@Override
	public String helpText() {
		return "{0} (node node ...)[:MT] [(creator)] : Creates an edge "
				+ "composed of two or more nodes, optionally in a particular "
				+ "microtheory and with an optional creator.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Matcher m = ARG_PATTERN.matcher(data);
		if (!m.matches()) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		Node creator = null;
		if (m.group(3) != null) {
			try {
				creator = dagHandler.getDAG().findOrCreateNode(m.group(3),
						creator);
			} catch (Exception e) {
				print("-1|Invalid creator node.\n");
				return;
			}
		}

		String microtheory = null;
		if (m.group(2) != null) {
			microtheory = m.group(2);
		}

		try {
			Node[] nodes = dagHandler.getDAG().parseNodes(
					m.group(1),
					creator,
					dagHandler.get(DAGPortHandler.DYNAMICALLY_ADD_NODES)
							.equals("true"), false);
			if (nodes == null) {
				print("-1|Problem parsing nodes.\n");
				return;
			}
			boolean[] flags = dagHandler
					.asBooleanArray(DAGPortHandler.EDGE_FLAGS);
			Edge edge = ((CycDAG) dagHandler.getDAG()).findOrCreateEdge(
					creator, nodes, microtheory, flags);

			if (edge instanceof ErrorEdge) {
				print("-1|" + ((ErrorEdge) edge).getError() + "\n");
			} else {
				DAGEdge dagEdge = (DAGEdge) edge;
				print(dagEdge.getID()
						+ "|"
						+ edge.toString(!dagHandler.get(
								DAGPortHandler.PRETTY_RESULTS).equals("true"))
						+ "|" + dagEdge.getCreator() + "|"
						+ dagEdge.getCreationDate() + "\n");
			}
		} catch (Exception e) {
			e.printStackTrace();
			print("-1|Problem parsing nodes.\n");
			return;
		}
	}
}
