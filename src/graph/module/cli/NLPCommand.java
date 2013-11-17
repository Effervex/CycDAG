package graph.module.cli;

import graph.core.DirectedAcyclicGraph;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.module.NLPToStringModule;

import org.apache.commons.lang3.StringUtils;

import core.Command;

public class NLPCommand extends Command {
	@Override
	public String helpText() {
		return "{0} [N/E/Q] [node/edge/query] : Given a node edge or query "
				+ "(distinguished by either N, E, or Q as the first argument), "
				+ "this command attempts to find the best natural language "
				+ "description of it.";
	}

	@Override
	public String shortDescription() {
		return "Produces a NLP description of a node, edge, or query.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DirectedAcyclicGraph dag = dagHandler.getDAG();
		NLPToStringModule nlpModule = (NLPToStringModule) dagHandler.getDAG()
				.getModule(NLPToStringModule.class);
		if (nlpModule == null) {
			print("NLP To String Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		// Parse the nodes
		int space = data.indexOf(' ');
		Object dagObject = null;
		String nlpData = data.substring(space + 1).trim();
		String result = null;
		if (data.substring(0, space).toUpperCase().equals("N")) {
			dagObject = dag.findOrCreateNode(nlpData, null, false, false, true);
			result = nlpModule.execute(dagObject);
		} else if (data.substring(0, space).toUpperCase().equals("E")) {
			if (nlpData.matches("\\d+"))
				dagObject = dag.getEdgeByID(Long.parseLong(nlpData));
			else
				dagObject = dag.parseNodes(nlpData, null, false, false);
			result = nlpModule.execute(dagObject);
		} else if (data.substring(0, space).toUpperCase().equals("Q")) {
			dagObject = new QueryObject(dag.parseNodes(nlpData, null, false, true));
			result = StringUtils.capitalize(nlpModule.execute(dagObject));
		}

		if (result == null)
			print("-1|Could not convert into natural language.\n");
		else
			print(StringUtils.capitalize(result) + "\n");
	}
}
