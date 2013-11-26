package graph.module.cli;

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.module.NLPToStringModule;

import java.util.ArrayList;

import util.UtilityMethods;
import core.Command;

public class NLPCommand extends Command {
	@Override
	public String helpText() {
		return "{0} N/E/Q node/edge/query [markup] : Given a node edge or query "
				+ "(distinguished by either N, E, or Q as the first argument), "
				+ "this command attempts to find the best natural language "
				+ "description of it. Optional markup argument for marking "
				+ "up natural text representations in [[Wiki syntax|Syntax]].";
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
			print("-1|NLP To String Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		// Parse markup
		ArrayList<String> args = UtilityMethods.split(data, ' ');
		String typeStr = args.get(0);
		String nlpData = args.get(1);
		boolean markup = false;
		if (args.size() >= 3)
			markup = (args.get(2).equalsIgnoreCase("T")) ? true : false;

		// Parse the nodes
		Object dagObject = null;
		String result = null;
		if (typeStr.equals("N")) {
			dagObject = dag.findOrCreateNode(nlpData, null, false, false, true);
			result = nlpModule.execute(markup, dagObject);
		} else if (typeStr.equals("E")) {
			if (nlpData.matches("\\d+"))
				dagObject = dag.getEdgeByID(Long.parseLong(nlpData));
			else
				dagObject = dag.parseNodes(nlpData, null, false, false);
			result = nlpModule.execute(markup, dagObject);
		} else if (typeStr.equals("Q")) {
			Node[] nodes = dag.parseNodes(nlpData, null, false, true);
			if (nodes != null) {
				dagObject = new QueryObject(nodes);
				result = nlpModule.execute(markup, dagObject);
			}
		}

		if (result == null)
			print("-1|Could not convert into natural language.\n");
		else
			print("1|" + result + "\n");
	}
}
