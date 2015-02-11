package graph.module.cli;

import graph.core.cli.DAGPortHandler;
import graph.module.ConceptNetAnalyserV2Module;
import util.UtilityMethods;
import core.Command;

public class DiscoverDisjointnessCommand extends Command {
	@Override
	public String helpText() {
		return "{0} folder assert? : Given a folder containing ConceptNet5 "
				+ "data files, this discovers disjointness from the relations "
				+ "and optionally asserts them immediately.";
	}

	@Override
	public String shortDescription() {
		return "Infers disjointness from a provided ConceptNet5 data repository.";
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
		if (split.length < 1) {
			print("-1|Please provide at least a folder path and optional "
					+ "T/F for asserting.\n");
			return;
		}
		
		String folderPath = (split[0].startsWith("\"")) ? UtilityMethods.shrinkString(split[0], 1) : split[0];
		boolean assertDisj = (split.length >= 2 && split[1].equalsIgnoreCase("T"));
		
		print("Discovering disjointness...\n");
		discoverModule.execute(folderPath, assertDisj);
	}
}
