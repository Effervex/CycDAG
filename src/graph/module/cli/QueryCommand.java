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

import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.cli.CollectionCommand;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;
import java.util.Map;

public class QueryCommand extends CollectionCommand {
	@Override
	public String helpText() {
		return "{0} (X Y ...) [A,B) : Poses a query to the DAG "
				+ "in the form of a bracketed edge expression "
				+ "consisting of either nodes (ID or named) "
				+ "or variables (in the form ?X or ? for "
				+ "'don't care' variables).\nThe query returns "
				+ "the number of valid substitutions (0 if there "
				+ "are none), followed by the variable substitutions.";
	}

	@Override
	public String shortDescription() {
		return "Poses a query to the DAG which returns a variable substitution map.";
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

		Node[] args = null;
		args = dagHandler.getDAG().parseNodes(data, null, false, false);
		if (args == null) {
			print("-1|Could not parse arguments.\n");
			return;
		}

		QueryObject qo = new QueryObject(true, false, QueryResult.ALL, args);
		Collection<Substitution> substitutions = queryModule.executeQuery(qo);

		// Sort
		substitutions = dagHandler.postProcess(substitutions, rangeStart_,
				rangeEnd_, true);

		if (qo.getResultState() == QueryResult.NIL) {
			print("0|NIL\n");
			return;
		} else if (qo.getResultState() == QueryResult.FALSE) {
			ErrorEdge rejectionReason = qo.getRejectionReason();
			if (rejectionReason != null)
				print("0|F|" + rejectionReason.toString(false) + "\n");
			else
				print("0|F\n");
			return;
		} else {
			if (qo.isProof()) {
				print("1|T|\n");
				return;
			}

			int size = substitutions.size();
			print(size + "|");
		}

		// Print
		for (Substitution sub : substitutions) {
			Map<String, Node> varSub = sub.getSubstitutionMap();
			boolean first = true;
			for (Map.Entry<String, Node> entry : varSub.entrySet()) {
				if (entry.getKey().startsWith("??"))
					continue;
				if (!first)
					print(",");
				print(entry.getKey() + "/"
						+ dagHandler.textIDObject(entry.getValue()));
				first = false;
			}
			print("|");
		}
		print("\n");
	}
}
