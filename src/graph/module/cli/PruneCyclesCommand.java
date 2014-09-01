package graph.module.cli;

import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.cli.DAGPortHandler;
import graph.module.TransitiveIntervalSchemaModule;

import java.io.BufferedReader;
import java.util.Collection;

import util.Pair;
import core.Command;

public class PruneCyclesCommand extends Command {
	@Override
	public String helpText() {
		return "{0} : Prompt the knowledge engineer to prune cyclic "
				+ "edges. The user will receive prompts for every cycle "
				+ "and remove an edge by selecting the edge number for "
				+ "every found cycle.";
	}

	@Override
	public String shortDescription() {
		return "Prompt the knowledge engineer to prune cyclic edges.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		CycDAG dag = (CycDAG) dagHandler.getDAG();
		TransitiveIntervalSchemaModule module = (TransitiveIntervalSchemaModule) dag
				.getModule(TransitiveIntervalSchemaModule.class);
		if (module == null) {
			print("-1|Transitive Interval Schema Module is not in use for this DAG.\n");
			return;
		}

		print("Scanning DAG for transitive cycles (this may take a few minutes)... ");
		flushOut();

		// Scan the DAG
		Collection<Pair<DAGEdge, DAGEdge>> cycles = module.identifyCycles(dag
				.getNodes());
		print(cycles.size() + " found!\n");

		// For every cycle
		BufferedReader in = getPortHandler().getReader();
		for (Pair<DAGEdge, DAGEdge> cycleEdges : cycles) {
			try {
				print("Remove which edge ('0' to ignore)?\n1: "
						+ cycleEdges.objA_.toString(false) + "\n2: "
						+ cycleEdges.objB_.toString() + "\nEnter number: ");
				flushOut();
				String index = in.readLine().trim();
				int decision = Integer.parseInt(index);
				DAGEdge edge = null;
				if (decision == 1)
					edge = cycleEdges.objA_;
				else if (decision == 2)
					edge = cycleEdges.objB_;

				// Removing the edge
				if (edge != null) {
					if (!dag.removeEdge(edge))
						print("Could not removed edge " + cycleEdges.objA_
								+ "!\n");
					print("Removed edge " + cycleEdges.objA_ + "\n");
				}
			} catch (Exception e) {
				print("Error! Proceeding to next cycle.\n");
			}
		}
	}

}
