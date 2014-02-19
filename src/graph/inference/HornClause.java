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

import graph.core.Node;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class HornClause {
	public static final double VARIABLE_WEIGHT = 10;
	public static final double NUM_LITERALS = 10;
	public static final double DEPTH_WEIGHT = 0.1;
	private Literal head_;
	private Literal[] body_;
	private Map<Literal, Double> bodyWeights_;

	public HornClause(Literal head, Literal... body) {
		head_ = head;
		body_ = body;
	}

	public double hornWeight(int depth) {
		bodyWeights_ = new HashMap<>();
		double sumWeight = 0;
		Collection<VariableNode> variables = new HashSet<>();
		for (Literal bodyLit : body_) {
			double bodyWeight = 0;
			Collection<VariableNode> litVars = new HashSet<>();
			for (Node node : bodyLit.getNodes()) {
				if (node instanceof VariableNode) {
					litVars.add((VariableNode) node);
				}
			}
			bodyWeight += litVars.size() * VARIABLE_WEIGHT;

			// bodyWeight += expansionWeight(bodyLit);
			bodyWeights_.put(bodyLit, bodyWeight);

			variables.addAll(litVars);
		}
		sumWeight += variables.size() * VARIABLE_WEIGHT;
		sumWeight += body_.length * NUM_LITERALS;
		sumWeight += depth * DEPTH_WEIGHT;
		return sumWeight;
	}

	public Literal getHead() {
		return head_;
	}

	@Override
	protected HornClause clone() {
		Literal newHead = head_;
		Literal[] newBody = null;
		if (body_ != null)
			newBody = Arrays.copyOf(body_, body_.length);
		return new HornClause(newHead, newBody);
	}

	public HornClause applySubstitution(Substitution substitution) {
		if (substitution.getSubstitutionMap().isEmpty())
			return this.clone();

		Literal newHead = (head_ == null) ? null : head_
				.applySubstitution(substitution);
		Literal[] newBody = applySubstitution(body_, substitution);
		return new HornClause(newHead, newBody);
	}

	public Literal[] getBody() {
		return body_;
	}

	@Override
	public String toString() {
		return head_ + " <- " + Arrays.toString(body_);
	}

	public static Literal[] applySubstitution(Literal[] literals,
			Substitution sub) {
		if (literals != null) {
			Literal[] newLits = new Literal[literals.length];
			for (int i = 0; i < literals.length; i++) {
				newLits[i] = literals[i].applySubstitution(sub);
			}
			return newLits;
		}
		return null;
	}
}
