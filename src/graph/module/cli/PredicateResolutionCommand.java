/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *    Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.module.cli;

import java.util.Collection;

import util.collection.WeightedSet;

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.PredicateResolutionModule;

public class PredicateResolutionCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} (EDGE) : Computes the most appropriate specialised "
				+ "predicate for a given edge. Note that this is approximate.";
	}

	@Override
	public String shortDescription() {
		return "Returns the most appropriate predicate specialisation for an edge.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		PredicateResolutionModule predResModule = (PredicateResolutionModule) dagHandler
				.getDAG().getModule(PredicateResolutionModule.class);
		if (predResModule == null) {
			print("-1|Predicate Resolution Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Node[] args = dagHandler.getDAG().parseNodes(data, null, false, false);
		WeightedSet<DAGNode> predicates = predResModule
				.execute((Object[]) args);
		if (predicates == null) {
			print("-1|Could not compute sub-predicates.\n");
			return;
		}
		Collection<DAGNode> sorted = dagHandler.postProcess(predicates,
				rangeStart_, rangeEnd_);
		print(predicates.size() + "");
		for (DAGNode predicate : sorted) {
			print("|" + dagHandler.textIDObject(predicate) + ","
					+ predicates.getWeight(predicate));
		}
		print("\n");
	}
}
