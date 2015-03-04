package graph.core.cli;

import java.util.ArrayList;

import util.UtilityMethods;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.Node;

public class CycRemoveEdgeCommand extends RemoveEdgeCommand {
	@Override
	public String helpText() {
		return "{0} edgeID forceRemove? : Removes an edge from the DAG, "
				+ "without retaining if necessary.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		ArrayList<String> split = UtilityMethods.split(data, ' ');
		if (split.size() > 2) {
			print("-1|Remove edge requires an edge and optional boolean argument!\n");
			return;
		}

		DAGEdge edge = null;
		boolean forceRemove = false;
		if (split.get(0).matches("\\d+")) {
			int id = Integer.parseInt(split.get(0));
			edge = dagHandler.getDAG().getEdgeByID(id);
		} else {
			Node[] nodes = dagHandler.getDAG().parseNodes(split.get(0), null,
					false, false);
			if (nodes != null) {
				edge = (DAGEdge) dagHandler.getDAG().findEdge(nodes);
			}
		}
		
		if (edge == null) {
			print("-1|Could not parse edge.\n");
			return;
		}

		if (split.size() == 2 && split.get(1).equalsIgnoreCase("T"))
			forceRemove = true;

		dagHandler.getDAG().writeCommand("removeedge " + data);
		if (((CycDAG) dagHandler.getDAG()).removeEdge(edge, forceRemove))
			print("1|Edge successfully removed.\n");
		else
			print("-1|Could not remove edge.\n");
	}
}
