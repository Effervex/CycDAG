package graph.module.cli;

import java.util.ArrayList;

import util.UtilityMethods;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.CommonQuery;
import graph.module.QueryModule;
import core.Command;

public class ImpactCommand extends Command {
	@Override
	public String helpText() {
		return "{0} col1 col2 ... : Returns the impact for collections "
				+ "in the form genls:isa:total, with one entry per "
				+ "collection and a sum entry for all collections.";
	}

	@Override
	public String shortDescription() {
		return "Returns the impact (number of children) of a collection(s).";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DirectedAcyclicGraph dag = dagHandler.getDAG();
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

		ArrayList<String> split = UtilityMethods.split(data, ' ');
		// For every collection
		print(split.size() + "|");
		int sumGenls = 0;
		int sumIsa = 0;
		for (String coll : split) {
			Node n = dag.findDAGNode(coll);
			int numGenls = CommonQuery.SPECS.runQuery(dag, n).size();
			int numIsa = CommonQuery.INSTANCES.runQuery(dag, n).size();
			print(Math.log(numGenls) + ":" + Math.log(numIsa) + ":"
					+ Math.log(numGenls + numIsa) + "|");
			sumGenls += numGenls;
			sumIsa += numIsa;
		}
		print(Math.log(sumGenls) + ":" + Math.log(sumIsa) + ":"
				+ Math.log(sumGenls + sumIsa) + "|\n");
	}
}
