/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.module.cli;

import java.util.Collection;

import graph.core.DAGEdge;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.MicrotheoryModule;

public class MicrotheoryCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} microtheory : Returns all nodes using a "
				+ "given microtheory. This is not transitive "
				+ "- just a property map.";
	}

	@Override
	public String shortDescription() {
		return "Returns all nodes using a particular microtheory.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		MicrotheoryModule microtheoryModule = (MicrotheoryModule) dagHandler
				.getDAG().getModule(MicrotheoryModule.class);
		if (microtheoryModule == null) {
			print("-1|Microtheory Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Collection<DAGEdge> edges = microtheoryModule.execute(data);
		if (edges == null) {
			print("0|");
			return;
		}

		// Apply sorting
		edges = dagHandler.postProcess(edges, rangeStart_, rangeEnd_);

		print(edges.size() + "|");
		for (DAGEdge e : edges)
			print(dagHandler.textIDObject(e) + "|");
		print("\n");
	}
}
