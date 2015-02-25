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

import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class AndWorker extends QueryWorker {
	private static final long serialVersionUID = 3867218930317989260L;
	private static final int LIMIT = 100000;

	public AndWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// Break the 'and' into separate queries
		Node[] nodes = queryObj.getNodes();
		List<QueryObject> components = new ArrayList<>();
		Collection<QueryObject> evaluatables = new HashSet<>();
		for (int i = 1; i < nodes.length; i++) {
			if (nodes[i] instanceof OntologyFunction) {
				QueryObject subQO = new QueryObject(queryObj.shouldJustify(),
						((OntologyFunction) nodes[i]).getNodes());
				components.add(subQO);
				if (isEvaluatable(subQO))
					evaluatables.add(subQO);
			} else if (!nodes[i].equals(PrimitiveNode.parseNode("true")))
				return;
		}

		// Continue until every OntFunc evaluated (or early exit)
		Collection<String> boundVariables = new HashSet<>();
		Collection<Substitution> intersect = null;
		while (!components.isEmpty()) {
			// Find the QOs with the fewest free variables and most bound
			// variables.
			Collection<QueryObject> lowQOs = new ArrayList<>();
			int minFree = Integer.MAX_VALUE;
			int maxBound = 0;
			for (QueryObject component : components) {
				// Count the free variables.
				int numFree = 0;
				int numBound = 0;
				for (VariableNode v : component.getVariables()) {
					if (!boundVariables.contains(v.toString()))
						numFree++;
					else
						numBound++;
				}

				// Special case for evaluatables - run as soon as possible,
				// otherwise ignore
				if (evaluatables.contains(component)) {
					if (numFree == 0) {
						lowQOs.clear();
						lowQOs.add(component);
						minFree = 0;
						break;
					} else
						continue;
				}

				if (numFree < minFree
						|| (numFree == minFree && numBound > maxBound)) {
					lowQOs.clear();
					minFree = numFree;
					maxBound = numBound;
				}
				if (numFree == minFree && numBound == maxBound) {
					lowQOs.add(component);
				}
			}

			// Run the lowest QOs
			int subSize = -1;
			QueryObject smallestQO = null;
			for (QueryObject lowQO : lowQOs) {
				// Run the query if not run yet
				if (!lowQO.isRun()) {
					lowQO.setToComplete(intersect);
					querier_.executeQuery(true, lowQO);
				} else {
					// Set up an intersect operation
					maxBound = 0;
				}

				// Find the lowest substitution size query
				int lowSize = lowQO.getResults().size();
				if (subSize == -1 || lowSize < subSize) {
					smallestQO = lowQO;
					subSize = lowSize;
					if (subSize == 0)
						break;
				}
			}

			// Integrate the smallest substitution set of the low
			// TODO No limit.
			Collection<Substitution> results = smallestQO.getResults();
			// If the query is false, stop now
			if (smallestQO.getResultState() == QueryResult.FALSE) {
				queryObj.addResults(false, results);
				queryObj.setRejectionReason(smallestQO.getRejectionReason());
				return;
			}

			// Only intersect if there are no bound variables
			if (maxBound == 0)
				intersect = intersectResults(intersect, results, LIMIT);
			else
				intersect = results;
			for (VariableNode vn : smallestQO.getVariables())
				boundVariables.add(vn.toString());

			// Quit if no proof is found or no variables can match.
			if (queryObj.isProof() && results == null || intersect.isEmpty())
				return;

			// Add justification
			if (queryObj.shouldJustify()) {
				if (!queryObj.getJustification().isEmpty())
					queryObj.getJustification().add(new Node[0]);
				queryObj.getJustification().addAll(
						smallestQO.getJustification());
			}

			// Remove from choices
			components.remove(smallestQO);
		}

		// Add the intersect results
		if (queryObj.isProof()) {
			queryObj.addResult(true, new Substitution());
		} else
			queryObj.addResults(true, intersect);
	}

	/**
	 * If this query predicate is an evaluatable predicate
	 *
	 * @param component
	 *            The query to check.
	 * @return True if the query predicate is evaluatable.
	 */
	private boolean isEvaluatable(QueryObject component) {
		String predName = component.getNode(0).getName();
		for (String evalPred : EvaluatablePredicateWorker.SUPPORTED_PREDICATES) {
			if (predName.equals(evalPred))
				return true;
		}
		return false;
	}

	public static Collection<Substitution> intersectResults(
			Collection<Substitution> intersect, Collection<Substitution> subs,
			int resultsLimit) {
		if (intersect == null)
			return subs;
		else if (subs == null)
			return intersect;
		else {
			// Not quite as simple as a retain all, as different variables can
			// be present.
			int numResults = 0;
			Collection<Substitution> newIntersect = new ArrayList<>(
					intersect.size());
			// For each intersect substitution
			for (Substitution subI : intersect) {
				// For each result substitution
				for (Substitution subS : subs) {
					// For each variable
					Map<String, Node> subMap = subS.getSubstitutionMap();
					boolean keepSub = true;
					for (String key : subMap.keySet()) {
						if (subI.containsVariable(key)
								&& !subI.getSubstitution(key).equals(
										subMap.get(key))) {
							keepSub = false;
							break;
						}
					}
					// Keep the sub, adding any new variable matches
					if (keepSub) {
						Substitution newSub = new Substitution(
								subI.getSubstitutionMap());
						newSub.getSubstitutionMap().putAll(subMap);
						newIntersect.add(newSub);
						numResults++;
						if (numResults >= resultsLimit)
							return newIntersect;
					}
				}
			}
			// intersect.retainAll(subs);
			return newIntersect;
		}
	}
}
