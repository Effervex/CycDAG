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
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class AndWorker extends QueryWorker {
	private static final long serialVersionUID = 3867218930317989260L;

	public AndWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		// Order the args to resolve smallest first
		Node[] nodes = queryObj.getNodes();
		List<Node> listNodes = Arrays.asList(nodes);
		List<OntologyFunction> ordered = orderNodes(nodes);
		if (ordered == null)
			return;

		Collection<Substitution> intersect = null;
		@SuppressWarnings("unchecked")
		List<Node[]>[] justifications = new List[ordered.size()];
		for (OntologyFunction func : ordered) {
			QueryObject funcObject = new QueryObject(func.getNodes());
			funcObject.setToComplete(intersect);
			Collection<Substitution> subs = querier_.execute(funcObject);
			justifications[listNodes.indexOf(func) - 1] = funcObject
					.getJustification();

			// Intersect results
			intersect = intersectResults(intersect, subs);

			// Quit if no proof is found or no variables can match.
			if (queryObj.isProof() && subs == null || intersect.isEmpty())
				return;
		}

		for (List<Node[]> justification : justifications) {
			if (justification != null) {
				if (!queryObj.getJustification().isEmpty())
					queryObj.getJustification().add(new Node[0]);
				queryObj.getJustification().addAll(justification);
			}
		}

		if (queryObj.isProof()) {
			queryObj.addResult(new Substitution());
		} else
			queryObj.addResults(intersect);
	}

	private List<OntologyFunction> orderNodes(Node[] nodes) {
		List<OntologyFunction> ordered = new ArrayList<>();
		for (int i = 1; i < nodes.length; i++) {
			if (nodes[i] instanceof OntologyFunction)
				ordered.add((OntologyFunction) nodes[i]);
			else if (!nodes[i].equals(PrimitiveNode.parseNode("true")))
				return null;
		}
		Collections.sort(ordered, new NumAssertionsComparator(relatedModule_,
				dag_));
		return ordered;
	}

	public static Collection<Substitution> intersectResults(
			Collection<Substitution> intersect, Collection<Substitution> subs) {
		if (intersect == null)
			return subs;
		else if (subs == null)
			return intersect;
		else {
			// Not quite as simple as a retain all, as different variables can
			// be present.
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
					}
				}
			}
			// intersect.retainAll(subs);
			return newIntersect;
		}
	}
}
