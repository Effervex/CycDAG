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
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import core.Command;

public class JustifyCommand extends Command {
	private static final Pattern BINDING_PATTERN = Pattern
			.compile("(\\?\\w+)/([^\\s,]+)");
	private static final Pattern ARG_PATTERN = Pattern
			.compile("^(\\(.+?\\))(\\s+(,?" + BINDING_PATTERN.pattern()
					+ ")+)?$");

	@Override
	public String helpText() {
		return "{0} (X Y ...) [binding] : Poses a query to the DAG "
				+ "in the form of a bracketed edge expression "
				+ "consisting of nodes (ID or named).\n"
				+ "If true, a justification of why it is true "
				+ "based on known edges is returned, "
				+ "or NIL if it is false.";
	}

	@Override
	public String shortDescription() {
		return "Returns the justification for why a query is true.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		QueryModule queryModule = (QueryModule) dagHandler.getDAG().getModule(
				QueryModule.class);
		if (queryModule == null) {
			print("Query module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Matcher m = ARG_PATTERN.matcher(data);
		if (!m.matches()) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		// Apply bindings
		String query = m.group(1);
		if (m.group(2) != null) {
			Matcher vm = BINDING_PATTERN.matcher(m.group(2));
			while (vm.find())
				query = query.replaceAll(Pattern.quote(vm.group(1)),
						vm.group(2));
		}
		Node[] args = dagHandler.getDAG().parseNodes(query, null, false, false);

		if (args == null) {
			print("-1|Could not parse arguments.\n");
			return;
		}
		QueryObject qo = new QueryObject(args);
		Collection<Substitution> substitutions = queryModule.execute(qo);
		if (substitutions == null || substitutions.isEmpty()) {
			print("0|NIL\n");
			return;
		} else {
			List<Node[]> justification = qo.getJustification();
			print(justification.size() + "|");
			for (Node[] edge : justification) {
				boolean first = true;
				if (edge.length != 0) {
					print("(");
					for (Node n : edge) {
						if (!first)
							print(" ");
						print(dagHandler.textIDObject(n));
						first = false;
					}
					print(")|");
				}
			}
			print("\n");
		}
	}

}
