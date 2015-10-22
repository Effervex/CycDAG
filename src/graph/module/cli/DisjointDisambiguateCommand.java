package graph.module.cli;

import java.util.ArrayList;
import java.util.Collection;

import util.UtilityMethods;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.DisjointDisambiguationModule;

/**
 * Clusters a collection of terms into consistent groups, such that each group
 * is consistent with one-another.
 *
 * @author Sam Sarjant
 */
public class DisjointDisambiguateCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{} terms... caseSensitive : Disambiguates a collection of "
				+ "terms, where each term can either be a node (name or ID), "
				+ "a text term (encased by \"s), or a special reference "
				+ "to an external resource (no refs yet). Also requires "
				+ "a T/F argument for case sensitivity when disambiguating "
				+ "text terms (defaults T).";
	}

	@Override
	public String shortDescription() {
		return "Disambiguates terms into consistent groups.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DisjointDisambiguationModule ddModule = (DisjointDisambiguationModule) dagHandler
				.getDAG().getModule(DisjointDisambiguationModule.class);
		if (ddModule == null) {
			print("-1|Disjoint Disambiguation Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		// Split the data
		ArrayList<String> split = UtilityMethods.split(data, ' ');
		// Extract case sensitive (if exists)
		String last = split.get(split.size() - 1);
		boolean caseSensitive = true;
		if (last.equalsIgnoreCase("T") || last.equalsIgnoreCase("F")) {
			split.remove(split.get(split.size() - 1));
			caseSensitive = last.equalsIgnoreCase("T");
		}

		// Parse the args
		Object[] args = new Object[split.size()];
		for (int i = 0; i < args.length; i++) {
			String value = split.get(i);
			if (value.matches("\".+\""))
				args[i] = UtilityMethods.shrinkString(value, 1);
			else {
				DAGNode node = (DAGNode) dagHandler.getDAG().findOrCreateNode(
						value, null, false, false, true);
				if (node == null) {
					print("-1|Could not parse '" + value + "' as node.\n");
					return;
				} else
					args[i] = node;
			}
		}

		// Run command
		Collection<Collection<DAGNode>> groups = ddModule.disambiguateGroups(
				args, caseSensitive);
		groups = dagHandler.postProcess(groups, rangeStart_, rangeEnd_, false);

		// Print data
		print(groups.size() + "|\n");
		for (Collection<DAGNode> group : groups) {
			group = dagHandler.postProcess(group, 0, Integer.MAX_VALUE, true);
			print(group.size() + "|");
			for (DAGNode n : group)
				print(dagHandler.textIDObject(n) + "|");
			print("\n");
		}
	}
}
