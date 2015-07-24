package graph.module.cli;

import graph.core.cli.DAGPortHandler;
import graph.module.ConceptNetAnalyserV2Module;
import util.UtilityMethods;
import core.Command;

public class DiscoverDisjointnessCommand extends Command {
	@Override
	public String helpText() {
		return "{0} folder : Given a folder containing ConceptNet5 " + "data files, this discovers disjointness from the relations " + "and optionally asserts them immediately.";
	}

	@Override
	public String shortDescription() {
		return "Infers disjointness from a provided ConceptNet5 data repository.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		ConceptNetAnalyserV2Module discoverModule = (ConceptNetAnalyserV2Module) dagHandler.getDAG().getModule(ConceptNetAnalyserV2Module.class);
		if (discoverModule == null) {
			print("-1|ConceptNet Discovery Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		String[] split = UtilityMethods.splitToArray(data, ' ');
		if (split.length < 1) {
			print("-1|Please provide folder path to ConceptNet5 files.\n");
			return;
		}

		String folderPath = (split[0].startsWith("\"")) ? UtilityMethods.shrinkString(split[0], 1) : split[0];

		print("Discovering disjointness...\n");
		discoverModule.execute(folderPath);
	}
}
