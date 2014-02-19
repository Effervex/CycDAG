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

import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;

import java.util.ArrayList;

import util.UtilityMethods;
import core.Command;

public class ValidArgCommand extends Command {
	@Override
	public String helpText() {
		return "{0} predicate argNum arg : Checks if an argument for an "
				+ "edge predicate is semantically valid. Note that this DOES "
				+ "NOT perform any disjointness reasoning.";
	}

	@Override
	public String shortDescription() {
		return "Checks if the provided edge arguments are semantically valid.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		CycDAG dag = (CycDAG) dagHandler.getDAG();
		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		ArrayList<String> split = UtilityMethods.split(data, ' ');
		if (split.size() != 3) {
			print("-1|Wrong number of arguments.\n");
			return;
		}
		try {
			DAGNode predicate = (DAGNode) dagHandler.getDAG().findOrCreateNode(
					split.get(0), null, false, false, true, false);
			Integer argNum = Integer.parseInt(split.get(1).replaceAll("'", ""));
			Node arg = dagHandler.getDAG().findOrCreateNode(split.get(2), null);

			if (dag.singleArgCheck(predicate, argNum, arg))
				print("1\n");
			else
				print("0\n");
		} catch (Exception e) {
			print("-1|Problem parsing nodes.\n");
		}
	}

}
