package graph.module.cli;

import java.util.Collection;

import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.PredicateRefinerModule;
import core.Command;

public class PredicateRefinerCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} #evidence threshold : Initiates the refinement process, "
				+ "inferring constraints for all RefinablePredicates with at "
				+ "least #evidence assertions. The constraints are determined "
				+ "by the threshold value, where a value of 1 implies every "
				+ "edge should follow the constraint.";
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
		if (split.length != 2) {
			print("-1|Two arguments required: minimum number "
					+ "of assertions and threshold.\n");
			return;
		}

		Collection<DAGNode> refined = prModule.execute(
				Integer.parseInt(split[0]), Double.parseDouble(split[1]));
		// Apply sorting
		refined = dagHandler.postProcess(refined, rangeStart_, rangeEnd_, true);

		print(refined.size() + "|");
		for (DAGNode n : refined)
			print(dagHandler.textIDObject(n) + "|");
		print("\n");
	}

}
