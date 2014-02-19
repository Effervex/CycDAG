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
package graph.core.cli;

import graph.core.CycDAG;
import graph.core.Identifiable;
import core.Command;

public class RandomCycNodeCommand extends Command {
	@Override
	public String helpText() {
		return "{0} [T/F] : Returns a random node, with optional "
				+ "parameter 'allowFunctions'. Defaults to true.";
	}

	@Override
	public String shortDescription() {
		return "Returns a random node from the DAG.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		boolean allowFunctions = true;
		if (data.equals("F"))
			allowFunctions = false;
		Identifiable obj = ((CycDAG) dagHandler.getDAG())
				.getRandomNode(allowFunctions);
		if (obj == null)
			print("-1|No nodes exist.\n");
		else
			print(dagHandler.textIDObject(obj) + "\n");
	}

}
