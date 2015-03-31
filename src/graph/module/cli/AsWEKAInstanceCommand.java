package graph.module.cli;

import org.apache.commons.lang3.StringUtils;

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.module.ConceptNetAnalyserV2Module;
import util.UtilityMethods;
import core.Command;

public class AsWEKAInstanceCommand extends Command {
	@Override
	public String helpText() {
		return "{0} conceptA conceptB (relation) : Given a pair of concepts "
				+ "and an optional ConceptNet5 relation, computes all "
				+ "computable features for the concepts. The relation "
				+ "allows relation-based features to be included in the "
				+ "output as well.";
	}

	@Override
	public String shortDescription() {
		return "Computes and returns a collection of features for a "
				+ "pair of concepts.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		ConceptNetAnalyserV2Module discoverModule = (ConceptNetAnalyserV2Module) dagHandler
				.getDAG().getModule(ConceptNetAnalyserV2Module.class);
		if (discoverModule == null) {
			print("-1|ConceptNet Discovery Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		String[] split = UtilityMethods.splitToArray(data, ' ');
		if (split.length < 2) {
			print("-1|Please provide two concepts and optional relation.\n");
			return;
		}

		Node conceptA = dagHandler.getDAG().findOrCreateNode(split[0], null,
				false, false, true);
		if (!(conceptA instanceof DAGNode)) {
			print("-1|" + split[0] + " could not be resolved as a DAGNode!\n");
			return;
		}
		Node conceptB = dagHandler.getDAG().findOrCreateNode(split[1], null,
				false, false, true);
		if (!(conceptB instanceof DAGNode)) {
			print("-1|" + split[1] + " could not be resolved as a DAGNode!\n");
			return;
		}
		String relation = (split.length == 3) ? split[2] : null;
		String[][] data = discoverModule.getARFFData(relation,
				(DAGNode) conceptA, (DAGNode) conceptB, null, true);
		print(data.length + "|");
		for (String[] instance : data) {
			print(StringUtils.join(instance, ',') + "|");
		}
		print("\n");
	}

}
