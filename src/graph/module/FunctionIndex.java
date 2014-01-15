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
package graph.module;

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.Collection;
import java.util.Set;

import util.collection.Trie;

public class FunctionIndex extends DAGModule<OntologyFunction> {
	private static final long serialVersionUID = -2107172826877208005L;

	private Trie<Node, OntologyFunction> index_;

	public FunctionIndex() {
		super();
		index_ = new Trie<>();
	}

	@Override
	public boolean addNode(DAGNode node) {
		if (node instanceof OntologyFunction) {
			return index_.put(((OntologyFunction) node).getNodes(), 0,
					(OntologyFunction) node);
		}
		return true;
	}

	@Override
	public void clear() {
		index_.clear();
	}

	@Override
	public OntologyFunction execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		return findFunction((Node[]) args);
	}

	public OntologyFunction findFunction(Node[] args) {
		Set<OntologyFunction> vals = index_.getValue(args, 0, null, false);
		if (vals == null || vals.isEmpty())
			return null;
		OntologyFunction func = vals.iterator().next();
		return func;
	}

	@Override
	public String toString() {
		return index_.toString();
	}

	public Collection<OntologyFunction> getAllFunctions(DAGNode functionNode) {
		return index_.getValue(new Node[] { functionNode }, 0, null, true);
	}
}
