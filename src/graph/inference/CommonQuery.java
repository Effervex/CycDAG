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
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import util.UtilityMethods;

public enum CommonQuery {
	ALIAS("(termStrings ?0 ?X)"),
	ALLGENLS("(genls ?0 ?X)"),
	ALLISA("(isa ?0 ?X)"),
	ARGNGENL("(argGenl ?0 ?1 ?X)", true),
	ARGNISA("(argIsa ?0 ?1 ?X)", true),
	COMMENT("(comment ?0 ?X)"),
	COMMONISA("(and (isa ?0 ?X) (isa ?1 ?X))"),
	COMMONGENLS("(and (genls ?0 ?X) (genls ?1 ?X))"),
	COMMONINSTANCES("(and (isa ?X ?0) (isa ?X ?1))"),
	COMMONSPECS("(and (genls ?X ?0) (genls ?X ?1))"),
	DIRECTGENLS("(assertedSentence (genls ?0 ?X))", true),
	DIRECTINSTANCE("(assertedSentence (isa ?X ?0))", true),
	DIRECTISA("(assertedSentence (isa ?0 ?X))", true),
	DIRECTSPECS("(assertedSentence (genls ?X ?0))", true),
	GENLSIBLINGS("(assertedSentence (genls ?0 ?X))", true),
	GENLPREDS("(genlPreds ?0 ?X)"),
	INSTANCES("(isa ?X ?0)"),
	ISASIBLINGS("(assertedSentence (isa ?0 ?X))", true),
	MAXINSTANCES("(isa ?X ?0)", true),
	MAXSPECS("(assertedSentence (genls ?X ?0))", true),
	MAXCOMMONINSTANCES("(and (isa ?X ?0) (isa ?X ?1))"),
	MAXCOMMONSPECS("(and (genls ?X ?0) (genls ?X ?1))"),
	MINARGNGENL("(argGenl ?0 ?1 ?X)", true),
	MINARGNISA("(argIsa ?0 ?1 ?X)", true),
	MINGENLS("(assertedSentence (genls ?0 ?X))", true),
	MINISA("(assertedSentence (isa ?0 ?X))", true),
	MINCOMMONISA("(and (isa ?0 ?X) (isa ?1 ?X))", true),
	MINCOMMONGENLS("(and (genls ?0 ?X) (genls ?1 ?X))", true),
	SPECPREDS("(genlPreds ?X ?0)"),
	SPECS("(genls ?X ?0)");

	private String queryStr_;
	private QueryObject queryObject_;

	private boolean specialQuery_;

	private CommonQuery(String query) {
		this(query, false);
	}

	private CommonQuery(String query, boolean specialQuery) {
		queryStr_ = UtilityMethods.shrinkString(query, 1);
		specialQuery_ = specialQuery;
	}

	public static void reset() {
		for (CommonQuery cq : values())
			cq.queryObject_ = null;
	}

	/**
	 * Converts the query string into a partially instantiated QueryObject.
	 *
	 * @param dag
	 *            The reference to the DAG.
	 */
	private void initialiseQueryObject(DirectedAcyclicGraph dag) {
		ArrayList<String> split = UtilityMethods.split(queryStr_, ' ');
		int i = 0;
		Node[] nodes = new Node[split.size()];
		for (String s : split)
			nodes[i++] = dag
					.findOrCreateNode(s, null, true, false, false, true);
		queryObject_ = new QueryObject(nodes);
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
		// Min args
		switch (this) {
		case ARGNISA:
		case MINARGNISA:
			// TODO Fix this.
			DAGNode argIsa = (DAGNode) dag.findOrCreateNode("arg"
					+ ((PrimitiveNode) args[1]).getPrimitive() + "Isa", null,
					false);
			QueryObject qo = new QueryObject(argIsa, args[0], new VariableNode(
					"?X"));
			for (Node n : QueryModule.parseResultsFromSubstitutions("?X",
					querier.executeQuery(qo)))
				if (!results.contains(n))
					results.add(n);
			break;
		case ARGNGENL:
		case MINARGNGENL:
			DAGNode argGenls = (DAGNode) dag.findOrCreateNode("arg"
					+ ((PrimitiveNode) args[1]).getPrimitive() + "Genl", null,
					false);
			qo = new QueryObject(argGenls, args[0], new VariableNode("?X"));
			for (Node n : QueryModule.parseResultsFromSubstitutions("?X",
					querier.executeQuery(qo)))
				if (!results.contains(n))
					results.add(n);
			break;
		default:
			break;
		}

		// Mins & maxes
		switch (this) {
		case GENLSIBLINGS:
		case MINGENLS:
			results.remove(args[0]);
		case ISASIBLINGS:
		case MINISA:
		case MINCOMMONISA:
		case MINCOMMONGENLS:
		case MINARGNISA:
		case MINARGNGENL:
			minGeneralFilter(results, dag);

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
			break;
		case MAXSPECS:
			results.remove(args[0]);
		case MAXINSTANCES:
		case MAXCOMMONINSTANCES:
		case MAXCOMMONSPECS:
			if (this == MAXINSTANCES
					&& querier.prove(false, CommonConcepts.ISA.getNode(dag),
							args[0],
							CommonConcepts.FIRST_ORDER_COLLECTION.getNode(dag)) == QueryResult.TRUE)
				return results;
			maxSpecFilter(results, dag);
			break;
		case DIRECTISA:
			if (args[0] instanceof OntologyFunction)
				results.addAll(querier.functionResults(
						(OntologyFunction) args[0], CommonConcepts.RESULT_ISA));
			break;
		case DIRECTGENLS:
			if (args[0] instanceof OntologyFunction)
				results.addAll(querier.functionResults(
						(OntologyFunction) args[0], CommonConcepts.RESULT_GENL));
			break;
		case DIRECTINSTANCE:
			if (args[0] instanceof OntologyFunction)
				results.addAll(querier.functionResults(
						(OntologyFunction) args[0], CommonConcepts.RESULT_ISA));
			break;
		case DIRECTSPECS:
			if (args[0] instanceof OntologyFunction)
				results.addAll(querier.functionResults(
						(OntologyFunction) args[0], CommonConcepts.RESULT_GENL));
			break;
		default:
			break;
		}
		return results;
	}

	/**
	 * Filter results to find the maximally general results
	 * 
	 * @param results
	 *            The results to filter.
	 * @param dag
	 *            The DAG access.
	 * @return The filtered input (same collection).
	 */
	public static Collection<Node> maxSpecFilter(Collection<Node> results,
			DirectedAcyclicGraph dag) {
		if (results.size() <= 1)
			return results;

		Set<Node> removed = new HashSet<>();
		for (Node node : results) {
			if (!removed.contains(node)) {
				// Get the genls
				Collection<Node> genls = CommonQuery.SPECS.runQuery(dag, node);
				genls.remove(node);

				// Remove any results that genls this node
				removed.addAll(genls);
			}
		}
		results.removeAll(removed);
		return results;
	}

	/**
	 * Filters results to find the minimally general results.
	 * 
	 * @param results
	 *            The results to filter.
	 * @param dag
	 *            The DAG access.
	 * @return The filtered input (same collection).
	 */
	public static Collection<? extends Node> minGeneralFilter(
			Collection<? extends Node> results, DirectedAcyclicGraph dag) {
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
		return results;
	}

	/**
	 * Runs this quick query with the given arguments. Returns the set of
	 * matching results, which is empty if no results found.
	 *
	 * @param dag
	 *            The dag access.
	 * @param args
	 *            The arguments of the query.
	 * @return The set of matching results, or empty if NIL.
	 */
	public Collection<Node> runQuery(DirectedAcyclicGraph dag, Node... args) {
		if (queryObject_ == null)
			initialiseQueryObject(dag);

		// Create a new QueryObject, replacing the placeholder variable
		Substitution sub = new Substitution();
		for (int j = 0; j < args.length; j++)
			sub.addSubstitution(new VariableNode("?" + j), args[j]);
		Node[] replacedArgs = sub.applySubstitution(queryObject_.getNodes());
		QueryObject qo = new QueryObject(false, false, QueryResult.TRUE,
				replacedArgs);

		QueryModule querier = (QueryModule) dag.getModule(QueryModule.class);

		Collection<Node> results = QueryModule.parseResultsFromSubstitutions(
				"?X", querier.executeQuery(qo));
		if (specialQuery_)
			results = runSpecial(results, querier, dag, args);
		return results;
	}
}
