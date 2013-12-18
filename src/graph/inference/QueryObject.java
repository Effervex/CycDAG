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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The query object is used as the single resource for storing query results and
 * presenting processed information about the query.
 * 
 * @author Sam Sarjant
 */
public class QueryObject {
	private ArrayList<DAGNode> atomics_;
	private int atomicIndex_ = -1;
	private ArrayList<DAGNode> completed_;
	private Set<DAGNode> completedSet_;
	private Node[] nodes_;
	private ArrayList<Substitution> results_;
	private Set<Substitution> resultsSet_;
	private ArrayList<VariableNode> variables_;
	private int variableIndex_ = -1;
	private Set<Substitution> toComplete_;
	private Substitution priorSubstitution_;
	private List<Node[]> justification_;

	public QueryObject(Node... nodes) {
		nodes_ = nodes;
		buildAtomicVariableSets(nodes);
		results_ = new ArrayList<>();
		resultsSet_ = new HashSet<>();
		completed_ = new ArrayList<>();
		completedSet_ = new HashSet<>();
		justification_ = new ArrayList<>();
	}

	private void buildAtomicVariableSets(Node... nodes) {
		atomics_ = new ArrayList<>();
		variables_ = new ArrayList<>();
		for (int i = 1; i < nodes.length; i++) {
			if (nodes[i] instanceof VariableNode
					&& !variables_.contains(nodes[i])) {
				variables_.add((VariableNode) nodes[i]);
				variableIndex_ = i;
			} else if (nodes[i] instanceof OntologyFunction) {
				if (!processFunction((OntologyFunction) nodes[i])) {
					atomics_.add((DAGNode) nodes[i]);
					if (atomicIndex_ == -1)
						atomicIndex_ = i;
					else
						variableIndex_ = i;
				}
			} else if (nodes[i] instanceof DAGNode
					&& !atomics_.contains(nodes[i])) {
				atomics_.add((DAGNode) nodes[i]);
				if (atomicIndex_ == -1)
					atomicIndex_ = i;
				else
					variableIndex_ = i;
			}
		}
	}

	/**
	 * Processes a function and extracts variables. Returns true if any
	 * variables are found.
	 * 
	 * @param func
	 *            The function to process.
	 * @return True if the function contains any variables.
	 */
	private boolean processFunction(OntologyFunction func) {
		boolean isVariable = false;
		for (Node n : func.getNodes()) {
			if (n instanceof VariableNode && !variables_.contains(n)) {
				isVariable = true;
				variables_.add((VariableNode) n);
			} else if (n instanceof OntologyFunction)
				isVariable |= processFunction((OntologyFunction) n);
		}
		return isVariable;
	}

	public void addCompleted(DAGNode n) {
		if (completedSet_.add(n))
			completed_.add(n);
	}

	/**
	 * Adds a result and creates substitutions where necessary.
	 * 
	 * @param nodes
	 *            The nodes being recorded and substituted.
	 * @return True if the inference can stop (either proof found, or stopping
	 *         parameter met).
	 */
	public boolean addResult(Node... nodes) {
		if (nodes.length != nodes_.length)
			return false;

		// Note the justification chain
		justification_.add(nodes);

		// Create a substitution
		Substitution s = new Substitution();
		for (int i = 0; i < nodes_.length; i++) {
			if (nodes_[i] instanceof VariableNode) {
				String varStr = nodes_[i].toString();
				if (s.containsVariable(varStr)
						&& s.getSubstitution(varStr) != nodes[i])
					return false;
				s.addSubstitution(varStr, nodes[i]);
			} else if (isProof() && !nodes[i].equals(nodes_[i]))
				return false;
		}
		return addResult(s);
	}

	/**
	 * Adds a result to the query. Returns true if the inference can stop.
	 * 
	 * @param substitution
	 *            The result to add.
	 * @return True if the inference can stop (either proof found, or stopping
	 *         parameter met).
	 */
	public boolean addResult(Substitution substitution,
			Node... justificationNodes) {
		if (justificationNodes != null && justificationNodes.length > 0)
			justification_.add(justificationNodes);

		if (isProof()) {
			// Check proof.
			if (!substitution.isEmpty())
				substitution = new Substitution();
			if (priorSubstitution_ != null)
				substitution = priorSubstitution_;
			results_.add(substitution);
			resultsSet_.add(substitution);
			return true;
		} else {
			if (priorSubstitution_ != null)
				substitution.getSubstitutionMap().putAll(
						priorSubstitution_.getSubstitutionMap());

			if (!resultsSet_.contains(substitution)) {
				results_.add(substitution);
				resultsSet_.add(substitution);
			}

			if (toComplete_ != null) {
				toComplete_.remove(substitution);
				return toComplete_.isEmpty();
			}
			return false;
		}
	}

	public void addResults(Collection<Substitution> results) {
		if (results == null)
			return;
		for (Substitution sub : results)
			if (addResult(sub))
				return;
	}

	public DAGNode getAtomic() {
		if (atomics_.size() == 0)
			return null;
		return atomics_.get(0);
	}

	public ArrayList<DAGNode> getCompleted() {
		return completed_;
	}

	public Node getNode(int i) {
		return nodes_[i];
	}

	public Node[] getNodes() {
		return nodes_;
	}

	public Collection<Substitution> getResults() {
		if (isProof() && results_.isEmpty())
			return null;
		else
			return results_;
	}

	public VariableNode getVariable() {
		return getVariable(0);
	}

	public VariableNode getVariable(int i) {
		if (variables_ == null || variables_.size() <= i)
			return null;
		return variables_.get(i);
	}

	public boolean isCompleted(Node n) {
		return completedSet_.contains(n);
	}

	public boolean isProof() {
		return variables_.isEmpty();
	}

	/**
	 * Creates a new QueryObject with different nodes but the same result sets,
	 * etc. However, does not copy justification!
	 * 
	 * @param nodes
	 *            The new nodes.
	 * @return The new QueryObject sharing the results.
	 */
	public QueryObject modifyNodes(Node... nodes) {
		QueryObject newObj = new QueryObject(nodes);
		newObj.completed_ = completed_;
		newObj.completedSet_ = completedSet_;
		newObj.results_ = results_;
		newObj.resultsSet_ = resultsSet_;
		newObj.toComplete_ = toComplete_;
		return newObj;
	}

	public QueryObject modifyNodes(Substitution baseSub, Node... nodes) {
		QueryObject qo = modifyNodes(nodes);
		qo.priorSubstitution_ = baseSub;
		return qo;
	}

	@Override
	public String toString() {
		return Arrays.toString(nodes_);
	}

	public void setToComplete(Collection<Substitution> intersect) {
		if (intersect != null)
			toComplete_ = new HashSet<>(intersect);
	}

	public int getAtomicIndex() {
		return atomicIndex_;
	}

	public int getVariableIndex() {
		return variableIndex_;
	}

	public Collection<Substitution> getPriorSubstitutions() {
		return toComplete_;
	}

	public int getNumVariables() {
		return variables_.size();
	}

	public List<Node[]> getJustification() {
		return justification_;
	}

	public void cleanTransitiveJustification(List<Node[]> justifications) {
		List<Node[]> transitiveJustification = new ArrayList<>();
		Node transitiveNode = null;
		for (int i = justifications.size() - 1; i >= 0; i--) {
			Node[] justNodes = justifications.get(i);
			if (transitiveNode == null
					|| justNodes[2].equals(transitiveNode)
					|| (justNodes[2] instanceof OntologyFunction && ((OntologyFunction) justNodes[2])
							.getNodes()[0].equals(transitiveNode))) {
				transitiveNode = justNodes[1];
				transitiveJustification.add(justNodes);
			}

			if (transitiveNode.equals(nodes_[1]))
				break;
		}

		Collections.reverse(transitiveJustification);
		justification_ = transitiveJustification;
	}
}
