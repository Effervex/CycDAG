package graph.module.cli;

import graph.core.CycDAG;
import graph.core.Identifiable;
import graph.core.cli.DAGPortHandler;
import core.Command;

public class RandomCycEdgeCommand extends Command {
	@Override
	public String helpText() {
		return "{0} [T/F] : Returns a random edge, with optional "
				+ "parameter 'allowFunctions' (in the edge). Defaults to true.";
	}

	@Override
	public String shortDescription() {
		return "Returns a random edge from the DAG.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		boolean allowFunctions = true;
		if (data.equals("F"))
			allowFunctions = false;
		Identifiable obj = ((CycDAG) dagHandler.getDAG())
				.getRandomEdge(allowFunctions);
		if (obj == null)
			print("-1|No edges exist.\n");
		else
			print(dagHandler.textIDObject(obj) + "\n");
	}

}
