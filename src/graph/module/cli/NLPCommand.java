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

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.module.NLPToStringModule;

import java.util.ArrayList;

import util.UtilityMethods;
import core.Command;

public class NLPCommand extends Command {
	@Override
	public String helpText() {
		return "{0} N/E/Q/M node/edge/query/markup [markup] [includeInput] : "
				+ "Given a node, edge, query, or marked-up string (distinguished "
				+ "by either N, E, Q, or M as the first argument), this command "
				+ "attempts to find the best natural language description of it. "
				+ "Optional boolean markup argument for marking up natural "
				+ "text representations in [[Wiki syntax|Syntax]]. Optional "
				+ "includeInput argument to include the input object in the output.";
	}

	@Override
	public String shortDescription() {
		return "Produces a NLP description of a node, edge, query, or marked-up string.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DirectedAcyclicGraph dag = dagHandler.getDAG();
		NLPToStringModule nlpModule = (NLPToStringModule) dag
				.getModule(NLPToStringModule.class);
		if (nlpModule == null) {
			print("-1|NLP To String Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		// Parse markup
		ArrayList<String> args = UtilityMethods.split(data, ' ');
		if (args.size() < 2) {
			print("-1|Invalid number of arguments.\n");
			return;
		}

		String typeStr = args.get(0);
		String nlpData = args.get(1).trim();
		boolean markup = false;
		boolean includeInput = false;
		if (args.size() >= 3)
			markup = (args.get(2).equalsIgnoreCase("T")) ? true : false;
		if (args.size() >= 4)
			includeInput = (args.get(3).equalsIgnoreCase("T")) ? true : false;

		// Parse the nodes
		Object dagObject = null;
		String result = null;
		if (typeStr.equals("N")) {
			dagObject = dag.findOrCreateNode(nlpData, null);
			result = nlpModule.execute(markup, dagObject);
		} else if (typeStr.equals("E")) {
			if (nlpData.matches("\\d+"))
				dagObject = dag.getEdgeByID(Long.parseLong(nlpData));
			else
				dagObject = dag.parseNodes(nlpData, null, false, false);
			result = nlpModule.execute(markup, dagObject);
		} else if (typeStr.equals("Q")) {
			Node[] nodes = dag.parseNodes(nlpData, null, false, false);
			if (nodes != null) {
				dagObject = new QueryObject(nodes);
				result = nlpModule.execute(markup, dagObject);
			}
		} else if (typeStr.equals("M")) {
			String string = nlpData;
			if (string.startsWith("\""))
				string = UtilityMethods.shrinkString(string, 1);
			result = nlpModule.execute(markup, string);
		}

		if (result == null)
			print("-1|Could not convert into natural language.\n");
		else {
			if (includeInput)
				print("1|" + result + ":" + nlpData + "\n");
			else
				print("1|" + result + "\n");
		}
	}
}
