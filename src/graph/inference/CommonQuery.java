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
package graph.inference;

import graph.core.CommonConcepts;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import util.UtilityMethods;

public enum CommonQuery {
	ALIAS("(termStrings $0 ?X)"),
	ALLGENLS("(genls $0 ?X)"),
	ALLISA("(isa $0 ?X)"),
	ARGNGENL("(or (argGenl $0 $1 ?X) (arg$1Genl $0 ?X))"),
	ARGNISA("(or (argIsa $0 $1 ?X) (arg$1Isa $0 ?X))"),
	COMMENT("(comment $0 ?X)"),
	DIRECTGENLS("(assertedSentence (genls $0 ?X))"),
	DIRECTINSTANCE("(assertedSentence (isa ?X $0))"),
	DIRECTISA("(assertedSentence (isa $0 ?X))"),
	DIRECTSPECS("(assertedSentence (genls ?X $0))"),
	DISJOINT("(disjointWith $0 $1)"),
	GENLSIBLINGS("(assertedSentence (genls $0 ?X))", true),
	INSTANCES("(isa ?X $0)"),
	ISASIBLINGS("(assertedSentence (isa $0 ?X))", true),
	MAXINSTANCES("(assertedSentence (isa ?X $0))", true),
	MAXSPECS("(assertedSentence (genls ?X $0))", true),
	MINARGNGENL("(or (argGenl $0 $1 ?X) " + "(arg$1Genl $0 ?X))", true),
	MINARGNISA("(or (argIsa $0 $1 ?X) " + "(arg$1Isa $0 ?X))", true),
	MINGENLS("(assertedSentence (genls $0 ?X))", true),
	MINISA("(assertedSentence (isa $0 ?X))", true),
	SPECS("(genls ?X $0)");

	private String queryStr_;

	private boolean specialQuery_;

	private CommonQuery(String query) {
		this(query, false);
	}

	private CommonQuery(String query, boolean specialQuery) {
		// TODO Convert the query to node ID form
		queryStr_ = UtilityMethods.shrinkString(query, 1);
		specialQuery_ = specialQuery;
	}

	/**
	 * Performs additional, query-specific, operations.
	 * 
	 * @param results
	 *            The results to alter.
	 * @param querier
	 *            The query module.
	 * @param dag
	 *            The DAG access.
	 * @param args
	 *            The args provided.
	 * @return Altered results, determined by special operations.
	 */
	private Collection<Node> runSpecial(Collection<Node> results,
			QueryModule querier, DirectedAcyclicGraph dag, Node[] args) {
		switch (this) {
		case GENLSIBLINGS:
		case MINGENLS:
			results.remove(args[0]);
		case ISASIBLINGS:
		case MINISA:
		case MINARGNISA:
		case MINARGNGENL:
			if (results.size() <= 1)
				return results;

			Set<Node> removed = new HashSet<>();
			for (Node node : results) {
				if (!removed.contains(node)) {
					// Get the genls
					Collection<Node> genls = CommonQuery.ALLGENLS.runQuery(dag,
							node);
					genls.remove(node);

					removed.addAll(genls);
				}
			}
			results.removeAll(removed);

			// Run the Siblings
			if (this == ISASIBLINGS || this == GENLSIBLINGS) {
				Collection<Node> parents = new ArrayList<>(results);
				results.clear();
				for (Node sibParent : parents) {
					CommonQuery downCQ = (this == ISASIBLINGS) ? CommonQuery.INSTANCES
							: CommonQuery.SPECS;
					results.addAll(downCQ.runQuery(dag, sibParent));
				}
				results.remove(args[0]);
			}

			return results;
		case MAXSPECS:
			results.remove(args[0]);
		case MAXINSTANCES:
			if (results.size() <= 1)
				return results;
			if (this == MAXINSTANCES
					&& querier.prove(CommonConcepts.ISA.getNode(dag), args[0],
							CommonConcepts.FIRST_ORDER_COLLECTION.getNode(dag)))
				return results;

			removed = new HashSet<>();
			for (Node node : results) {
				if (!removed.contains(node)) {
					// Get the genls
					Collection<Node> genls = CommonQuery.SPECS.runQuery(dag,
							node);
					genls.remove(node);

					// Remove any results that genls this node
					removed.addAll(genls);
				}
			}
			results.removeAll(removed);
			return results;
		default:
			break;
		}
		return results;
	}

	public Collection<Node> runQuery(DirectedAcyclicGraph dag, Node... args) {
		String[] nodeIDs = new String[args.length];
		for (int j = 0; j < nodeIDs.length; j++)
			nodeIDs[j] = args[j].getIdentifier();
		String query = UtilityMethods.replaceToken(queryStr_, nodeIDs);

		ArrayList<String> split = UtilityMethods.split(query, ' ');
		int i = 0;
		Node[] nodes = new Node[split.size()];
		for (String s : split) {
			nodes[i++] = dag.findOrCreateNode(s, null, false, false, false, true);
		}
		QueryObject qo = new QueryObject(nodes);
		QueryModule querier = (QueryModule) dag.getModule(QueryModule.class);

		Collection<Node> results = querier.executeAndParseVar(qo, "?X");
		if (specialQuery_)
			results = runSpecial(results, querier, dag, args);
		return results;
	}
}
