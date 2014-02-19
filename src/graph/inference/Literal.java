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
import graph.core.DAGObject;
import graph.core.NamedEdge;
import graph.core.Node;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class Literal extends NamedEdge {
	private static final long serialVersionUID = -2463520896161256321L;
	private Collection<VariableNode> vars_;
	private double weight_;

	public Literal(DAGNode predicate, Node... nodes) {
		super(predicate, nodes);
		weight_ = 0;
		vars_ = new HashSet<>();
		for (int i = 0; i < edgeNodes_.length; i++) {
			if (edgeNodes_[i] instanceof VariableNode) {
				vars_.add((VariableNode) edgeNodes_[i]);
				weight_ += 0.01 / (i + 1);
			}
		}
		weight_ += vars_.size();
	}

	public Literal applySubstitution(Substitution substitution) {
		if (substitution.getSubstitutionMap().isEmpty())
			return this;

		boolean changed = false;
		DAGNode newPred = (DAGNode) edgeNodes_[0];
		if (substitution.containsVariable(newPred.toString())) {
			newPred = (DAGNode) substitution.getSubstitution(newPred
					.toString());
			changed = true;
		}
		Node[] args = new Node[edgeNodes_.length - 1];
		for (int i = 1; i < edgeNodes_.length; i++) {
			args[i - 1] = edgeNodes_[i];
			if (substitution.containsVariable(args[i - 1].toString())) {
				args[i - 1] = substitution.getSubstitution(args[i - 1]
						.toString());
				changed = true;
			}
		}

		if (changed)
			return new Literal(newPred, args);
		else
			return this;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		Literal other = (Literal) obj;
		return Arrays.equals(edgeNodes_, other.edgeNodes_);
	}

	@Override
	public int compareTo(DAGObject o) {
		if (!(o instanceof Literal))
			return super.compareTo(o);
		Literal other = (Literal) o;
		int result = Double.compare(weight_, other.weight_);
		if (result != 0)
			return result;
		return edgeNodes_.toString().compareTo(other.toString());
	}

	public Collection<VariableNode> getVariables() {
		return vars_;
	}

	public double getWeight() {
		return weight_;
	}

	@Override
	public int hashCode() {
		return Arrays.hashCode(edgeNodes_);
	}

	public Literal normalised() {
		Map<VariableNode, VariableNode> theta = new HashMap<>();
		Node[] replacedNodes = new Node[edgeNodes_.length - 1];
		int index = 0;
		for (int i = 1; i < edgeNodes_.length; i++) {
			if (edgeNodes_[i] instanceof VariableNode) {
				if (!theta.containsKey(edgeNodes_[i]))
					theta.put((VariableNode) edgeNodes_[i], new VariableNode(
							"?X" + index++));
				replacedNodes[i - 1] = theta.get(edgeNodes_[i]);
			} else
				replacedNodes[i - 1] = edgeNodes_[i];
		}
		return new Literal(getEdgeName(), replacedNodes);
	}
}
