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
package graph.module;

import graph.core.Node;
import graph.inference.CommonQuery;

import java.util.Collection;
import java.util.HashSet;

import org.apache.commons.collections4.CollectionUtils;

/**
 * A module for calculating semantic similarity between two concepts.
 * 
 * @author Sam Sarjant
 */
public class SemanticSimilarityModule extends DAGModule<Float> {
	private static final long serialVersionUID = 5576091924279360723L;

	@Override
	public Float execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (args.length < 2)
			return -1f;
		if (args[0].equals(args[1]))
			return 1f;
		return semanticSimilarity((Node) args[0], (Node) args[1]);
	}

	/**
	 * Calculates the semantic similarity between two concepts.
	 * 
	 * @param nodeA
	 *            A concept.
	 * @param nodeB
	 *            A concept.
	 * @return The similarity between the two concepts.
	 */
	public float semanticSimilarity(Node nodeA, Node nodeB) {
		Collection<Node> parents1 = getParents(nodeA);
		return semanticSimilarity(parents1, nodeB);
	}

	public float semanticSimilarity(Collection<Node> parents1, Node nodeB) {
		Collection<Node> parents2 = getParents(nodeB);
		Collection<Node> intersect = CollectionUtils.intersection(parents1,
				parents2);
		Collection<Node> union = CollectionUtils.union(parents1, parents2);
		return 1f * intersect.size() / union.size();
	}

	public Collection<Node> getParents(Node node) {
		Collection<Node> parents = new HashSet<Node>(
				CommonQuery.ALLISA.runQuery(dag_, node));
		parents.addAll(CommonQuery.ALLGENLS.runQuery(dag_, node));
		return parents;
	}

}
