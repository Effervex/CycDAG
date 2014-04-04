/*******************************************************************************
 * Copyright (c) 2013 University of Waikato, Hamilton, New Zealand.
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

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.FunctionIndex;

public class FunctionExtentCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} functionNode : Returns all NART/NAUTs utilising "
				+ "the functionNode to define the concept.";
	}

	@Override
	public String shortDescription() {
		return "Returns the extent of a function's use.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		FunctionIndex functionModule = (FunctionIndex) dagHandler.getDAG()
				.getModule(FunctionIndex.class);
		if (functionModule == null) {
			print("-1|Function Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		boolean[] flags = dagHandler.asBooleanArray(DAGPortHandler.NODE_FLAGS);
		if (flags.length >= 1)
			flags[0] = false;
		Node functionNode = (DAGNode) dagHandler.getDAG().findOrCreateNode(
				data, null, flags);
		Collection<OntologyFunction> narts = functionModule
				.getAllFunctions((DAGNode) functionNode);
		if (narts == null) {
			print("0|");
			return;
		}

		// Apply sorting
		narts = dagHandler.postProcess(narts, rangeStart_, rangeEnd_);

		print(narts.size() + "|");
		for (OntologyFunction nart : narts)
			print(dagHandler.textIDObject(nart) + "|");
		print("\n");
	}
}
