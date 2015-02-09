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
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

public class OrWorker extends QueryWorker {
	private static final long serialVersionUID = -8075355634991901791L;

	public OrWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		if (!queryObj.getNode(0).getName().equals("or"))
			throw new IllegalArgumentException(
					"Predicate must be or for this module!");

		Node[] nodes = queryObj.getNodes();
		boolean allFalse = true;
		for (int i = 1; i < nodes.length; i++) {
			QueryObject funcObject = new QueryObject(queryObj.shouldJustify(),
					((OntologyFunction) nodes[i]).getNodes());
			querier_.executeQuery(true, funcObject);

			// Intersect results
			if (queryObj.getResultState() != QueryResult.FALSE) {
				allFalse = false;
				queryObj.addResults(true, funcObject.getResults());
				if (queryObj.shouldJustify())
					queryObj.getJustification().addAll(
							funcObject.getJustification());
			}
		}
		if (allFalse)
			queryObj.addResult(false, new Substitution());
	}
}
