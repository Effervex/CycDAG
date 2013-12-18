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
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

public class EqualsWorker extends QueryWorker {
	public EqualsWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private static final long serialVersionUID = -3119459864077118394L;

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		Node[] nodes = queryObj.getNodes();
		for (int i = 2; i < nodes.length; i++) {
			if (!nodes[i].equals(nodes[i - 1]))
				return;
		}
		queryObj.addResult(new Substitution(), nodes);
	}

}
