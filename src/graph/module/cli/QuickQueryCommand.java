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

import graph.core.Node;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.inference.CommonQuery;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import util.UtilityMethods;

public class QuickQueryCommand extends CollectionCommand {
	private static final Pattern ARG_PATTERN = Pattern.compile("^(\\S+)(.+?)$");

	@Override
	public String helpText() {
		StringBuffer buffer = new StringBuffer(
				"{0} <shortCommand> <nodeArgs> [A,B) : Poses a query "
						+ "to the DAG using the CommonQuery enum. "
						+ "Each query returns a set of nodes."
						+ "\nQueries include: ");
		boolean first = true;
		for (CommonQuery cq : CommonQuery.values()) {
			if (!first)
				buffer.append(", ");
			buffer.append(cq);
			first = false;
		}
		buffer.append(". See java file for full details.");
		return buffer.toString();
	}

	@Override
	public String shortDescription() {
		return "Poses a query to the dag using short syntax.";
	}

	@Override
	protected void executeImpl() {
		super.executeImpl();
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		QueryModule queryModule = (QueryModule) dagHandler.getDAG().getModule(
				QueryModule.class);
		if (queryModule == null) {
			print("-1|Query module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Matcher m = ARG_PATTERN.matcher(data.trim());
		if (!m.matches()) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		CommonQuery cq;
		try {
			cq = CommonQuery.valueOf(m.group(1).toUpperCase());
		} catch (IllegalArgumentException e) {
			print("-1|Quick Query not found.\n");
			return;
		}

		int firstSpace = data.indexOf(' ');
		if (firstSpace == -1) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		ArrayList<String> split = UtilityMethods.split(m.group(2).trim(), ' ');
		Node[] args = new Node[split.size()];
		for (int i = 0; i < args.length; i++) {
			args[i] = dagHandler.getDAG().findOrCreateNode(split.get(i), null);
			if (args[i] == null) {
				print("-1|Could not parse arguments.\n");
				return;
			}
		}

		Collection<Node> result = cq.runQuery(dagHandler.getDAG(), args);

		// Sort results
		result = dagHandler.sort(result, rangeStart_, rangeEnd_);

		if (result == null || result.isEmpty()) {
			print("0|NIL\n");
			return;
		} else {
			int size = result.size();
			print(size + "|");
		}

		for (Node n : result) {
			if (n != null)
				print(dagHandler.textIDObject(n));
			print("|");
		}
		print("\n");
	}

}
