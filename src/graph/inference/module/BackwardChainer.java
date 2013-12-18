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
package graph.inference.module;

import graph.core.DAGNode;
import graph.core.Node;
import graph.inference.HornClause;
import graph.inference.Literal;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import util.collection.MultiMap;

public class BackwardChainer extends QueryWorker {
	private static final long serialVersionUID = -3759651304054753667L;
	private static final double EPSILON = 0.000001d;
	private static final String UNIQUE_SUFFIX = "_unIQue_";
	private MultiMap<DAGNode, HornClause> headHCMap_;

	public BackwardChainer(QueryModule queryModule) {
		super(queryModule);
		headHCMap_ = MultiMap.createListMultiMap();
	}

	// /**
	// * Proves if a query is true or not.
	// *
	// * @param cnf
	// * The query posed in CNF.
	// * @return True if the query is true.
	// */
	// public boolean proveQuery(Literal... cnf) {
	// try {
	// ChainResult result = chainLiterals(cnf, 0, 0,
	// new HashSet<Literal>());
	// return result.result_;
	// } catch (IllegalAccessException e) {
	// e.printStackTrace();
	// }
	// return false;
	// }
	//
	// /**
	// * Asks for a set of variable bindings for a query.
	// *
	// * @param cnf
	// * The query posed in CNF.
	// * @return The set of substitutions.
	// */
	// public Collection<Substitution> askQuery(Literal... cnf) {
	// try {
	// // TODO Convert variables to unique values
	// cnf = uniqueVariables(cnf);
	//
	// ChainResult result = chainLiterals(cnf, 0, 0,
	// new HashSet<Literal>());
	// if (result.result_) {
	// return deUniqueVariables(result.substitutions_);
	// }
	// } catch (IllegalAccessException e) {
	// e.printStackTrace();
	// }
	// return null;
	// }

	private Collection<Substitution> deUniqueVariables(
			Collection<Substitution> substitutions) {
		if (substitutions == null)
			return null;
		Collection<Substitution> replacedSubs = new HashSet<>();
		for (Substitution sub : substitutions) {
			Map<String, Node> subMap = sub.getSubstitutionMap();
			Substitution replSub = new Substitution();
			for (String key : subMap.keySet()) {
				int uniqueIndex = key.indexOf(UNIQUE_SUFFIX);
				if (uniqueIndex != -1)
					replSub.addSubstitution(key.substring(0, uniqueIndex),
							subMap.get(key));
			}
			replacedSubs.add(replSub);
		}
		return replacedSubs;
	}

	private Literal uniqueVariables(Literal lit) {
		Node[] nodes = lit.getNodes();
		Node[] replacedNodes = new Node[nodes.length - 1];
		for (int j = 1; j < nodes.length; j++) {
			if (nodes[j] instanceof VariableNode)
				replacedNodes[j - 1] = new VariableNode(nodes[j].getName()
						+ UNIQUE_SUFFIX);
			else
				replacedNodes[j - 1] = nodes[j];
		}
		return new Literal(lit.getEdgeName(), replacedNodes);
	}

	private Literal[] uniqueVariables(Literal[] lits) {
		Literal[] replaced = new Literal[lits.length];
		for (int i = 0; i < lits.length; i++)
			replaced[i] = uniqueVariables(lits[i]);
		return replaced;
	}

	private ChainResult chainLiterals(Literal[] literals, int i, int depth,
			Set<Literal> trace) throws IllegalAccessException {
		if (literals.length == 0)
			return new ChainResult(true, new HashSet<Substitution>(0));

		if (i == 0)
			Arrays.sort(literals);
		Literal lit = literals[i];
		ChainResult result = proveLiteral(lit, depth, trace);
		// Stopping condition
		if (i == literals.length - 1)
			return result;

		if (!result.result_)
			return new ChainResult(false, null);
		else {
			if (result.substitutions_ == null
					|| result.substitutions_.isEmpty())
				return chainLiterals(literals, i + 1, depth, trace);

			ChainResult combinedSubResult = null;
			for (Substitution sub : result.substitutions_) {
				// Apply sub
				Literal[] subbedLiterals = HornClause.applySubstitution(
						literals, sub);
				ChainResult subResult = chainLiterals(subbedLiterals, i + 1,
						depth, trace);
				if (subResult.result_) {
					if (combinedSubResult == null)
						combinedSubResult = subResult;
					else
						combinedSubResult.substitutions_
								.addAll(subResult.substitutions_);
				}
			}

			if (combinedSubResult == null)
				return new ChainResult(false, null);
			else
				return combinedSubResult;
		}
	}

	private ChainResult proveLiteral(Literal query, int depth,
			Set<Literal> trace) throws IllegalAccessException {
		boolean isProveQuery = query.getVariables().isEmpty();

		// Check if DAG contains result
		Collection<Substitution> substitutions = querier_
				.execute((Object[]) query.getNodes());
		if (!substitutions.isEmpty() && isProveQuery)
			return new ChainResult(true, substitutions);

		// Check for loops
		Literal normalised = query.normalised();
		if (trace.contains(normalised)) {
			if (substitutions.isEmpty())
				return new ChainResult(false, null);
			else
				return new ChainResult(true, substitutions);
		}
		trace.add(normalised);

		// Find pertinent clauses
		Collection<HornClause> sortedClauses = findPertinentClauses(query,
				depth);

		// Recurse into ranked clauses
		for (HornClause clause : sortedClauses) {
			ChainResult clauseResults = chainLiterals(clause.getBody(), 0,
					depth + 1, trace);
			if (clauseResults.result_) {
				if (isProveQuery)
					return clauseResults;
				else
					substitutions.addAll(clauseResults.substitutions_);
			}
		}

		if (substitutions.isEmpty())
			return new ChainResult(false, null);
		else
			return new ChainResult(true, substitutions);
	}

	private class ChainResult {
		public boolean result_;
		public Collection<Substitution> substitutions_;

		public ChainResult(boolean result,
				Collection<Substitution> substitutions) {
			result_ = result;
			substitutions_ = substitutions;
		}

		@Override
		public String toString() {
			return result_ + " " + substitutions_;
		}
	}

	public Collection<HornClause> findPertinentClauses(Literal query, int depth)
			throws IllegalAccessException {
		List<HornClause> relatedClauses = headHCMap_.getList(query
				.getEdgeName());
		if (relatedClauses == null)
			return new ArrayList<>(0);
		return relatedClauses;
		// SortedMap<Double, HornClause> sortedClauses = new TreeMap<>();
		// if (relatedClauses != null) {
		// // Rank the clauses
		// for (HornClause clause : relatedClauses) {
		// MultiMap<String, Integer> queryIndices = QueryModule
		// .indexVariables(clause.getHead().getNodes());
		// Substitution revisedSubstitution = QueryModule
		// .createSubstitution(queryIndices, query);
		// if (revisedSubstitution != null) {
		// clause = clause.applySubstitution(revisedSubstitution);
		//
		// double weight = clause.hornWeight(depth);
		// while (sortedClauses.containsKey(weight))
		// weight += EPSILON;
		// sortedClauses.put(weight, clause);
		// }
		// }
		// }
		// return sortedClauses.values();
	}

	public void addRule(HornClause hc) {
		if (hc.getHead() != null)
			headHCMap_.put(hc.getHead().getEdgeName(), hc);
	}

	@Override
	public void queryInternal(QueryObject queryObj) {
		try {
			// TODO This method needs rewriting to conform with queryObj
			Node[] nodes = queryObj.getNodes();
			Literal query = new Literal((DAGNode) queryObj.getNode(0),
					Arrays.copyOfRange(nodes, 1, nodes.length));
			query = uniqueVariables(query);
			queryObj.addResults(deUniqueVariables(folBCOr(query, 0,
					new CachedResults())));
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Attempt to prove a goal using the KB knowledge and rules.
	 * 
	 * @param goal
	 *            The goal to prove.
	 * @param depth
	 *            The current depth.
	 * @param trace
	 *            The computed proofs thus far.
	 * @return A collection of substitutions which prove the goal, or null if no
	 *         solution possible.
	 * @throws IllegalAccessException
	 *             Should something go awry...
	 */
	private Collection<Substitution> folBCOr(Literal goal, int depth,
			CachedResults trace) throws IllegalAccessException {
		// Check for loops/use known results
		System.out.println(goal);
		Literal normalised = goal.normalised();
		Substitution normalSub = unify(goal, normalised);
		if (trace.contains(normalised)) {
			return trace.getResults(normalised, normalSub.inverseSub());
		}

		// TODO Get direct assertions
		// Collection<Substitution> results = querier_.applyModule(
		// "assertedSentence", new QueryObject(goal.getNodes()));
		// trace.addResults(normalised, null, normalSub);
		Collection<Substitution> results = null;

		// Run through the horn clauses
		// TODO order these heuristically.
		Collection<HornClause> sortedClauses = findPertinentClauses(goal, depth);
		for (HornClause hc : sortedClauses) {
			hc = standardiseHC(hc, depth);
			System.out.println("HC: " + hc);
			Collection<Substitution> hcResult = folBCAnd(hc.getBody(),
					unify(hc.getHead(), goal), depth, trace);
			if (hcResult != null) {
				results.addAll(hcResult);
			}
		}
		if (results.isEmpty())
			return null;
		trace.addResults(normalised, results, normalSub);
		return results;
	}

	private HornClause standardiseHC(HornClause hc, int standardVal) {
		Substitution sub = new Substitution();
		// TODO Really need to step through here, but this works for now.
		sub.addSubstitution("?X", new VariableNode("?Xstan" + standardVal));
		sub.addSubstitution("?Y", new VariableNode("?Ystan" + standardVal));
		sub.addSubstitution("?Z", new VariableNode("?Zstan" + standardVal));
		return hc.applySubstitution(sub);
	}

	/**
	 * Applies a substitution to a set of substitutions.
	 * 
	 * @param targets
	 *            The targets to apply the substitutions to.
	 * @param sub
	 *            The sub to apply.
	 * @return The target set with their substitutions modified per sub.
	 */
	public static Collection<Substitution> applySubstitution(
			Collection<Substitution> targets, Substitution sub) {
		if (targets == null)
			return null;
		if (sub == null)
			return targets;
		Collection<Substitution> newTargets = new HashSet<>();
		for (Substitution tarSub : targets) {
			newTargets.add(tarSub.applySubstitution(sub));
		}
		return newTargets;
	}

	/**
	 * Attempts to prove a conjoined set of literals, using a provided
	 * substitution.
	 * 
	 * @param goals
	 *            The goals to prove.
	 * @param substitution
	 *            The substitution to apply to the goals.
	 * @param depth
	 *            The current depth.
	 * @param trace
	 *            The computed proofs thus far...
	 * @return A collection of substitutions which prove the goal, or null if no
	 *         solution possible.
	 * @throws IllegalAccessException
	 *             Should something go awry...
	 */
	private Collection<Substitution> folBCAnd(Literal[] goals,
			Substitution substitution, int depth, CachedResults trace)
			throws IllegalAccessException {
		if (substitution == null)
			return null;
		if (goals.length == 0) {
			Collection<Substitution> single = new ArrayList<>(1);
			single.add(substitution);
			return single;
		}

		// Proves the first goal
		Literal first = goals[0];
		Literal[] rest = Arrays.copyOfRange(goals, 1, goals.length);
		Collection<Substitution> firstSubs = folBCOr(
				first.applySubstitution(substitution), depth + 1, trace);
		if (firstSubs == null)
			return null;

		// Recurses to the rest.
		Collection<Substitution> resultSubs = new HashSet<>();
		for (Substitution sub : firstSubs) {
			sub.getSubstitutionMap().putAll(substitution.getSubstitutionMap());
			Collection<Substitution> restResults = folBCAnd(rest, sub, depth,
					trace);
			if (restResults == null)
				return null;
			resultSubs.addAll(restResults);
		}
		return resultSubs;
	}

	private Substitution unify(Literal x, Literal y) {
		Substitution newSub = new Substitution();
		if (x.equals(y))
			return newSub;
		Node[] xNodes = x.getNodes();
		Node[] yNodes = y.getNodes();
		if (xNodes.length != yNodes.length)
			return null;

		for (int i = 0; i < xNodes.length; i++) {
			newSub = unify(xNodes[i], yNodes[i], newSub);
			if (newSub == null)
				return null;
		}
		return newSub;
	}

	private Substitution unify(Node x, Node y, Substitution sub) {
		if (x.equals(y))
			return sub;
		if (x instanceof VariableNode) {
			return unifyVar(x, y, sub);
		}
		if (y instanceof VariableNode) {
			return unifyVar(y, x, sub);
		}
		return null;
	}

	public Substitution unifyVar(Node x, Node y, Substitution sub) {
		if (sub.containsVariable(x.toString()))
			return unify(sub.getSubstitution(x.toString()), y, sub);
		else if (sub.containsVariable(y.toString()))
			return unify(x, sub.getSubstitution(y.toString()), sub);
		else {
			Substitution newSub = sub.clone();
			newSub.addSubstitution(x.toString(), y);
			return newSub;
		}
	}

	/**
	 * The stored partial or complete results, in normalised form.
	 * 
	 * @author Sam Sarjant
	 */
	private class CachedResults {
		private Map<Literal, Collection<Substitution>> results_ = new HashMap<>();

		public Collection<Substitution> getResults(Literal normalisedLit,
				Substitution unNormalSub) {
			Collection<Substitution> normalisedResults = results_
					.get(normalisedLit);
			return applySubstitution(normalisedResults, unNormalSub);
		}

		public boolean contains(Literal normalisedLit) {
			return results_.containsKey(normalisedLit);
		}

		public void addResults(Literal normalisedLit,
				Collection<Substitution> results, Substitution normalSub) {
			if (results == null || results.isEmpty())
				results_.put(normalisedLit, null);
			else
				// Store as normalised substitution.
				results_.put(normalisedLit,
						applySubstitution(results, normalSub));
		}

		@Override
		public String toString() {
			return results_.toString();
		}
	}
}
