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

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.HashMap;
import java.util.Map;

public class Substitution {
	private Map<String, Node> substitutionMap_;

	public Substitution() {
		substitutionMap_ = new HashMap<>();
	}

	public Substitution(Map<String, Node> substitutionMap) {
		substitutionMap_ = new HashMap<>(substitutionMap);
	}

	public Substitution(String var, DAGNode node) {
		substitutionMap_ = new HashMap<>();
		substitutionMap_.put(var, node);
	}

	public Substitution(Object variable, DAGNode otherNode) {
		this(variable.toString(), otherNode);
	}

	public void addSubstitution(String variable, Node substitution) {
		substitutionMap_.put(variable, substitution);
	}

	public void addSubstitution(Object variable, Node substitution) {
		addSubstitution(variable.toString(), substitution);
	}

	public Node getSubstitution(Object variable) {
		return substitutionMap_.get(variable.toString());
	}

	@Override
	public Substitution clone() {
		Substitution clone = new Substitution(substitutionMap_);
		return clone;
	}

	public Map<String, Node> getSubstitutionMap() {
		return substitutionMap_;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((substitutionMap_ == null) ? 0 : substitutionMap_.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Substitution other = (Substitution) obj;
		if (substitutionMap_ == null) {
			if (other.substitutionMap_ != null)
				return false;
		} else if (!substitutionMap_.equals(other.substitutionMap_))
			return false;
		return true;
	}

	public boolean containsVariable(String var) {
		return substitutionMap_.containsKey(var);
	}

	@Override
	public String toString() {
		return substitutionMap_.toString();
	}

	public Substitution inverseSub() {
		Substitution inverse = new Substitution();
		for (String key : substitutionMap_.keySet()) {
			// TODO Inefficient!
			inverse.addSubstitution(substitutionMap_.get(key).toString(),
					new VariableNode(key));
		}
		return inverse;
	}

	public Substitution applySubstitution(Substitution sub) {
		Substitution subbedSub = new Substitution();
		for (String key : substitutionMap_.keySet()) {
			Node value = substitutionMap_.get(key);
			key = (sub.containsVariable(key)) ? sub.getSubstitution(key)
					.toString() : key;
			subbedSub.addSubstitution(key, value);
		}
		return subbedSub;
	}

	public boolean isEmpty() {
		return substitutionMap_.isEmpty();
	}

	/**
	 * Applies a substitution to a set of nodes to produce a new set of nodes.
	 * 
	 * @param sub
	 *            The substitution to apply.
	 * @return A new array of nodes if the substitution changed anything,
	 *         otherwise return the same input set.
	 */
	public Node[] applySubstitution(Node[] nodes) {
		Node[] subbedNodes = new Node[nodes.length];
		boolean changed = false;
		for (int i = 0; i < subbedNodes.length; i++) {
			subbedNodes[i] = applySubstitution(nodes[i]);
			changed |= !subbedNodes[i].equals(nodes[i]);
		}
		if (changed)
			return subbedNodes;
		return nodes;
	}

	public Node applySubstitution(Node node) {
		if (node instanceof OntologyFunction) {
			return new OntologyFunction(
					applySubstitution(((OntologyFunction) node).getNodes()));
		} else if (substitutionMap_.containsKey(node.toString()))
			return substitutionMap_.get(node.toString());
		return node;
	}
}
