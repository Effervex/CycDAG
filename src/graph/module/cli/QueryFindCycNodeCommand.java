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

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import util.AliasedObject;

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.Substitution;
import graph.module.QueryModule;

public class QueryFindCycNodeCommand extends FindCycNodeCommand {
	private static final Pattern ARG_PATTERN = Pattern
			.compile("^(?:\".+\"|[^\"]\\S+).+?(\\(.+?\\?X.*\\))");

	@Override
	public String helpText() {
		return "{0} alias [caseSensitive] [exactString] [showMatch] <QUERY> : "
				+ "Performs the same node search as findnodes, but "
				+ "also uses a query to filter the results such that "
				+ "only those that satisfy the query remain. Use '?X' "
				+ "where the found node will be substituted in the query.";
	}

	@Override
	public String shortDescription() {
		return "Finds all nodes that match a string and satisfy a query.";
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Collection<Object> findNodes(DAGPortHandler dagHandler) {
		QueryModule qm = (QueryModule) dagHandler.getDAG().getModule(
				QueryModule.class);
		if (qm == null) {
			print("-1|Query module is not in use for this DAG.\n");
			return null;
		}

		// Parse nodes
		Collection<Object> nodes = super.findNodes(dagHandler);

		// Parse the query and filter
		Node[] args = null;
		String queryArg = null;
		Matcher m = ARG_PATTERN.matcher(data);
		if (m.matches())
			queryArg = m.group(1);
		if (queryArg == null) {
			print("-1|Could not find query.\n");
			return null;
		}
		args = dagHandler.getDAG().parseNodes(queryArg, null, false, false);
		if (args == null) {
			print("-1|Could not parse query arguments.\n");
			return null;
		}

		Collection<Object> filteredNodes = new ArrayList<>();
		for (Object n : nodes) {
			DAGNode node = null;
			if (n instanceof DAGNode)
				node = (DAGNode) n;
			else if (n instanceof AliasedObject)
				node = ((AliasedObject<Character, DAGNode>) n).object_;
			Substitution substitution = new Substitution("?X", node);
			boolean satisfies = qm.prove(substitution.applySubstitution(args));
			if (satisfies) {
				filteredNodes.add(n);
			}
		}
		return filteredNodes;
	}
}
