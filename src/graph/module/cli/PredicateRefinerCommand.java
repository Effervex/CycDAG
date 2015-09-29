package graph.module.cli;

import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.PredicateRefinerModule;

import java.util.Collection;

public class PredicateRefinerCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} #evidence threshold assert? sourceKey (predicate) : Initiates "
				+ "the refinement process, inferring constraints for all "
				+ "RefinablePredicates with at least #evidence assertions "
				+ "and either asserts or outputs the constraints. "
				+ "The constraints are determined by the threshold value, "
				+ "where a value of 1 implies every edge should follow the "
				+ "constraint. Counts may use the source key (enter null) "
				+ "for no key to merge duplicate sourced assertions. Optional "
				+ "predicate narrows command to a single predicate.";
	}

	@Override
	public String shortDescription() {
		return "Initiates predicate refinement for RefinablePredicates.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DirectedAcyclicGraph dag = dagHandler.getDAG();
		PredicateRefinerModule prModule = (PredicateRefinerModule) dag
				.getModule(PredicateRefinerModule.class);
		if (prModule == null) {
			print("-1|Predicate Refiner Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		String[] split = data.split("\\s+");
		if (split.length < 4) {
			print("-1|At least four arguments required: minimum number "
					+ "of assertions, threshold, whether to assert, the "
					+ "duplicate merging source property key, and "
					+ "optional predicate.\n");
			return;
		}

		boolean assertConstraints = split[2].equalsIgnoreCase("T");
		String sourceKey = (split[3].equals("null")) ? null : split[3];
		String predicate = (split.length == 5) ? split[4] : null;
		Collection<Edge> refined = prModule.refinePredicates(
				Integer.parseInt(split[0]), Float.parseFloat(split[1]),
				assertConstraints, sourceKey, predicate);
		// Apply sorting
		refined = dagHandler.postProcess(refined, rangeStart_, rangeEnd_, true);

		print(refined.size() + "|");
		for (Edge e : refined)
			print(dagHandler.textIDObject(e) + "|");
		print("\n");
	}
}
