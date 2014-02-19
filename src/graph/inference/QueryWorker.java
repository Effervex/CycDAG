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

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.module.ModuleException;
import graph.module.OntologyEdgeModule;
import graph.module.QueryModule;

import java.io.Serializable;
import java.util.Collection;

public abstract class QueryWorker implements Serializable {
	private static final long serialVersionUID = -9204653084421990460L;

	protected QueryModule querier_;

	protected transient OntologyEdgeModule relatedModule_;

	protected transient DirectedAcyclicGraph dag_;

	public QueryWorker(QueryModule queryModule) {
		querier_ = queryModule;
	}

	public Collection<Substitution> query(Node... nodes)
			throws IllegalArgumentException {
		QueryObject queryObj = new QueryObject(nodes);
		queryInternal(queryObj);
		return queryObj.getResults();
	}

	/**
	 * Asks a query containing one or more variables and returns all possible
	 * substitutions for the variables.
	 * 
	 * @param queryObj
	 *            The necessary information and returned object for the query.
	 */
	public abstract void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException;

	public void setDAG(DirectedAcyclicGraph dag) {
		dag_ = dag;
		relatedModule_ = (OntologyEdgeModule) dag
				.getModule(OntologyEdgeModule.class);
		if (relatedModule_ == null)
			throw new ModuleException(
					"Cannot perform query without related edge module.");
	}
}
