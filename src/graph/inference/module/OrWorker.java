/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand
 ******************************************************************************/
package graph.inference.module;

import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
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
		for (int i = 1; i < nodes.length; i++) {
			QueryObject funcObject = new QueryObject(
					((OntologyFunction) nodes[i]).getNodes());
			querier_.execute(funcObject);

			// Intersect results
			queryObj.addResults(funcObject.getResults());
			queryObj.getJustification().addAll(funcObject.getJustification());
		}
	}
}
