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

import graph.core.CommonConcepts;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.module.QueryModule;

import java.util.Arrays;

public class NotWorker extends QueryWorker {

	public NotWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private static final long serialVersionUID = -1858194911287526128L;

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		if (!queryObj.getNode(0).equals(CommonConcepts.NOT.getNode(dag_)))
			throw new IllegalArgumentException(
					"Can only reason over 'not' queries.");

		// TODO Apply De Morgan to simplify nested joining functions (and/or)

		// Special case for genls & disjointWith
		OntologyFunction negated = (OntologyFunction) queryObj.getNode(1);
		if (negated.getNodes()[0].equals(CommonConcepts.GENLS.getNode(dag_))) {
			Node[] disjointNodes = Arrays.copyOf(negated.getNodes(),
					negated.getNodes().length);
			disjointNodes[0] = CommonConcepts.DISJOINTWITH.getNode(dag_);
			QueryObject disjoint = queryObj.modifyNodes(disjointNodes);
			querier_.execute(disjoint);
		} else if (negated.getNodes()[0].equals(CommonConcepts.DISJOINTWITH
				.getNode(dag_))) {
			Node[] genlsNodes = Arrays.copyOf(negated.getNodes(),
					negated.getNodes().length);
			genlsNodes[0] = CommonConcepts.GENLS.getNode(dag_);
			QueryObject disjoint = queryObj.modifyNodes(genlsNodes);
			querier_.execute(disjoint);
			if (disjoint.getResults() != null)
				return;

			// Swap genls around
			Node temp = genlsNodes[1];
			genlsNodes[1] = genlsNodes[2];
			genlsNodes[2] = temp;
			disjoint = queryObj.modifyNodes(genlsNodes);
			querier_.execute(disjoint);
		}

		// Every other case always returns NIL, unless there is an explicit
		// 'not' assertion.
		return;
	}
}
