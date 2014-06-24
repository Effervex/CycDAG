/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *    Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.module;

import java.util.Collection;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.PrimitiveNode;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import util.collection.WeightedSet;

public class PredicateResolutionModule extends DAGModule<WeightedSet<DAGNode>> {
	private static final long serialVersionUID = 1L;

	@Override
	public WeightedSet<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		if (args.length < 1 || !(args[0] instanceof DAGNode))
			return null;
		DAGNode basePred = (DAGNode) args[0];
		Node[] nodes = new Node[args.length - 1];
		for (int i = 1; i < args.length; i++) {
			if (!(args[i] instanceof Node))
				return null;
			nodes[i - 1] = (Node) args[i];
		}
		return getLikelyPredicates(basePred, nodes);
	}

	/**
	 * Resolve the most appropriate specialised predicate, based on argument
	 * constraints.
	 * 
	 * @param basePred
	 *            The base predicate to specialise.
	 * @param args
	 *            The arguments of the base predicate
	 * @return A weighted set of all lower predicates, weighted by closeness of
	 *         arg constraints.
	 */
	public WeightedSet<DAGNode> getLikelyPredicates(DAGNode basePred,
			Node... args) {
		QueryModule qm = (QueryModule) dag_.getModule(QueryModule.class);
		if (qm == null)
			return null;
		WeightedSet<DAGNode> predicates = new WeightedSet<>();
		float predDist = predicateDistance(basePred, args);
		if (predDist > 0)
			predicates.add(basePred, predDist);
		VariableNode var = new VariableNode("?X");
		QueryObject qo = new QueryObject(
				CommonConcepts.GENLPREDS.getNode(dag_), var, basePred);

		Collection<Substitution> subs = qm.execute(qo);
		for (Substitution sub : subs) {
			DAGNode pred = (DAGNode) sub.getSubstitution(var);
			predDist = predicateDistance(pred, args);
			if (predDist > 0)
				predicates.add(pred, predDist);
		}
		predicates.normaliseWeightTo1();
		return predicates;
	}

	private float predicateDistance(DAGNode pred, Node[] args) {
		CycDAG cycDAG = (CycDAG) dag_;
		SemanticSimilarityModule semsimMod = (SemanticSimilarityModule) cycDAG
				.getModule(SemanticSimilarityModule.class);
		float similarity = 0;
		for (int i = 0; i < args.length; i++) {
			if (!cycDAG.singleArgCheck(pred, (i + 1), args[i]))
				return 0;
			Collection<Node> argIsa = CommonQuery.MINARGNISA.runQuery(dag_,
					pred, PrimitiveNode.parseNode((i + 1) + ""));
			Collection<Node> argGenls = CommonQuery.MINARGNGENL.runQuery(dag_,
					pred, PrimitiveNode.parseNode((i + 1) + ""));
			if (argIsa.isEmpty() && argGenls.isEmpty()) {
				similarity += 0.00001f;
				continue;
			}

			for (Node constraint : argIsa)
				similarity += semsimMod.semanticSimilarity(args[i], constraint)
						/ argIsa.size();
			for (Node constraint : argGenls)
				similarity += semsimMod.semanticSimilarity(args[i], constraint)
						/ argGenls.size();
		}
		return similarity / args.length;
	}
}
