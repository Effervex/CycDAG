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

import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.EdgeModifier;
import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import util.ObjectWrapper;

/**
 * The query object is used as the single resource for storing query results and
 * presenting processed information about the query.
 * 
 * @author Sam Sarjant
 */
public class QueryObject {
	/** The index of the first atomic argument. */
	private int atomicIndex_ = -1;
	/** All atomic arguments in the query. */
	private ArrayList<DAGNode> atomics_;
	/** All DAGNodes that have been processed. */
	private ArrayList<DAGNode> completed_;
	/** All DAGNodes that have been processed in set form. */
	private Set<DAGNode> completedSet_;
	/** The justification for the results. */
	private List<Node[]> justification_;
	/** If a justification is required. */
	private boolean needJustification_;
	/** The nodes of the query. */
	private Node[] nodes_;
	/** Any prior substitutions to apply to the nodes. */
	private Substitution priorSubstitution_;
	/** The substitutions for a result. */
	private SortedSet<Substitution> results_;
	/** The substitutions for a result in set form. */
	private Set<Substitution> resultsSet_;
	/** The substitutions yet to be matched. */
	private Set<Substitution> toComplete_;
	/** The index of the first variable argument. */
	private int variableIndex_ = -1;
	/** All variable arguments in the query. */
	private ArrayList<VariableNode> variables_;
	/** The state of the results - if TRUE, FALSE, or NIL. */
	private ObjectWrapper<QueryResult> resultState_ = new ObjectWrapper<>(
			QueryResult.NIL);
	/** The reason for a query's rejection (if rejected). */
	private ErrorEdge rejectionReason_;
	/** If the query has already been run. */
	private boolean hasRun_ = false;
	/** If the arguments should be verified. */
	private boolean verifyArguments_ = true;
	/** The query should only be run when the goal result can be attained. */
	private QueryResult goalResult_;

	// TODO Implement a 'cares about' variable so queries can be skipped if they
	// don't produce the cared about answer.

	/**
	 * Constructor for a new QueryObject. This QO is initialised with no
	 * justification.
	 * 
	 * @param nodes
	 *            The nodes of the query.
	 */
	public QueryObject(Node... nodes) {
		this(false, false, QueryResult.ALL, nodes);
	}

	/**
	 * Constructor for a new QueryObject with optional justification.
	 * 
	 * @param needJustification
	 *            If a justification is required.
	 * @param goalResult
	 *            The goal result for this query (i.e. what are we
	 *            interpreting?). Default to ALL.
	 * @param nodes
	 *            The nodes of the query.
	 */
	public QueryObject(boolean shouldVerify, boolean needJustification,
			QueryResult goalResult, Node... nodes) {
		verifyArguments_ = shouldVerify;
		nodes_ = nodes;
		buildAtomicVariableSets(nodes);
		results_ = new TreeSet<>();
		resultsSet_ = new HashSet<>();
		completed_ = new ArrayList<>();
		completedSet_ = new HashSet<>();
		needJustification_ = needJustification;
		justification_ = new ArrayList<>();
		goalResult_ = goalResult;
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
	 * @param isTrueResult
	 *            If the result being added is a positive example (i.e. true),
	 *            or a negative example (i.e. proof against).
	 * @param nodes
	 *            The nodes being recorded and substituted.
	 * @return True if the inference can stop (either proof found, or stopping
	 *         parameter met).
	 */
	public boolean addResult(boolean isTrueResult, Node... nodes) {
		Node[] nonNegNodes = EdgeModifier.getUnmodNodes(nodes, CycDAG.selfRef_);
		if (nonNegNodes.length != nodes_.length)
			return false;

		// Note the justification chain
		if (needJustification_)
			justification_.add(nodes);

		// Create a substitution
		Substitution s = new Substitution();
		for (int i = 0; i < nodes_.length; i++) {
			if (nodes_[i] instanceof VariableNode) {
				String varStr = nodes_[i].toString();
				if (s.containsVariable(varStr)
						&& s.getSubstitution(varStr) != nonNegNodes[i])
					return false;
				s.addSubstitution(varStr, nonNegNodes[i]);
			} else if (isProof() && !nonNegNodes[i].equals(nodes_[i]))
				return false;
		}
		return addResult(isTrueResult, s);
	}

	/**
	 * Adds a result to the query. Returns true if the inference can stop.
	 * 
	 * @param isTrueResult
	 *            If the result being added is a positive example (i.e. true),
	 *            or a negative example (i.e. proof against).
	 * @param substitution
	 *            The result to add.
	 * @param justificationNodes
	 *            The justification to note for the result.
	 * @return True if the inference can stop (either proof found, or stopping
	 *         parameter met).
	 */
	public boolean addResult(boolean isTrueResult, Substitution substitution,
			Node... justificationNodes) {
		if (needJustification_ && justificationNodes != null
				&& justificationNodes.length > 0)
			justification_.add(justificationNodes);

		// OR based truth
		if (isTrueResult)
			resultState_.object_ = QueryResult.TRUE;
		else if (resultState_.object_ == QueryResult.NIL)
			resultState_.object_ = QueryResult.FALSE;
		if (isProof()) {
			if (isTrueResult) {
				// Check proof.
				if (!substitution.isEmpty())
					substitution = new Substitution();
				if (priorSubstitution_ != null)
					substitution = priorSubstitution_;
				results_.add(substitution);
				resultsSet_.add(substitution);
			}
			return true;
		} else {
			if (priorSubstitution_ != null && substitution != null)
				substitution.getSubstitutionMap().putAll(
						priorSubstitution_.getSubstitutionMap());

			if (isTrueResult && !resultsSet_.contains(substitution)) {
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

	/**
	 * Adds a result to the query with a full justification provided.
	 *
	 * @param isTrueResult
	 *            If the result being added is a positive example (i.e. true),
	 *            or a negative example (i.e. proof against).
	 * @param substitution
	 *            The result to add.
	 * @param justification
	 *            The justification list to add to the justification.
	 */
	public void addResult(boolean isTrueResult, Substitution substitution,
			List<Node[]> justification) {
		addResult(isTrueResult, substitution);
		if (needJustification_ && justification != null
				&& justification.size() > 0)
			justification_.addAll(justification);
	}

	public void addResults(boolean isTrueResults,
			Collection<Substitution> results) {
		if (results == null)
			return;
		for (Substitution sub : results)
			if (addResult(isTrueResults, sub))
				return;
	}

	public void cleanTransitiveJustification(List<Node[]> justifications,
			DirectedAcyclicGraph dag) {
		if (!needJustification_)
			return;
		List<Node[]> transitiveJustification = new ArrayList<>();
		Node transitiveNode = null;
		for (int i = justifications.size() - 1; i >= 0; i--) {
			Node[] justNodes = justifications.get(i);
			// Remove negation
			Node[] nonNegatedNodes = EdgeModifier.getUnmodNodes(justNodes, dag);
			if (transitiveNode == null
					|| nonNegatedNodes[2].equals(transitiveNode)
					|| (nonNegatedNodes[2] instanceof OntologyFunction && ((OntologyFunction) nonNegatedNodes[2])
							.getNodes()[0].equals(transitiveNode))) {
				transitiveNode = nonNegatedNodes[1];
				transitiveJustification.add(justNodes);
			}

			if (transitiveNode.equals(nodes_[1]))
				break;
		}

		Collections.reverse(transitiveJustification);
		justification_ = transitiveJustification;
	}

	public DAGNode getAtomic() {
		if (atomics_.size() == 0)
			return null;
		return atomics_.get(0);
	}

	public int getAtomicIndex() {
		return atomicIndex_;
	}

	public ArrayList<DAGNode> getCompleted() {
		return completed_;
	}

	public List<Node[]> getJustification() {
		return justification_;
	}

	public Node getNode(int i) {
		return nodes_[i];
	}

	public Node[] getNodes() {
		return nodes_;
	}

	public int getNumVariables() {
		return variables_.size();
	}

	public Collection<Substitution> getPriorSubstitutions() {
		return toComplete_;
	}

	public Collection<Substitution> getResults() {
		if (isProof()
				&& (results_.isEmpty() || getResultState() != QueryResult.TRUE))
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

	public int getVariableIndex() {
		return variableIndex_;
	}

	public Collection<VariableNode> getVariables() {
		return variables_;
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
	 * 
	 * @return The new QueryObject sharing the results.
	 */
	public QueryObject modifyNodes(Node... nodes) {
		QueryObject newObj = new QueryObject(verifyArguments_,
				needJustification_, goalResult_, nodes);
		newObj.completed_ = completed_;
		newObj.completedSet_ = completedSet_;
		newObj.results_ = results_;
		newObj.resultsSet_ = resultsSet_;
		newObj.toComplete_ = toComplete_;
		newObj.resultState_ = resultState_;
		newObj.priorSubstitution_ = priorSubstitution_;
		return newObj;
	}

	public QueryObject clone() {
		return modifyNodes(nodes_);
	}

	public QueryObject modifyNodes(Substitution baseSub, Node... nodes) {
		QueryObject newObj = new QueryObject(verifyArguments_,
				needJustification_, goalResult_, nodes);
		newObj.results_ = results_;
		newObj.resultsSet_ = resultsSet_;
		newObj.toComplete_ = toComplete_;
		newObj.priorSubstitution_ = baseSub;
		return newObj;
	}

	public void setToComplete(Collection<Substitution> intersect) {
		if (intersect != null) {
			toComplete_ = new HashSet<>(intersect);
			// results_.clear();
			// completed_.clear();
			// completedSet_.clear();
			// resultsSet_.clear();
			// resultState_.object_ = QueryResult.NIL;
			// justification_.clear();
		}
	}

	@Override
	public String toString() {
		return Arrays.toString(nodes_);
	}

	public boolean shouldJustify() {
		return needJustification_;
	}

	/**
	 * Gets the state of the executed query object - TRUE, FALSE, or NIL.
	 *
	 * @return The result of the executed query object.
	 */
	public QueryResult getResultState() {
		return resultState_.object_;
	}

	/**
	 * Sets the state of the executed query object - TRUE, FALSE, or NIL.
	 */
	public void setResultState(QueryResult result) {
		resultState_.object_ = result;
	}

	public void flipResultState() {
		if (resultState_.object_ == QueryResult.TRUE) {
			resultState_.object_ = QueryResult.FALSE;
			results_.clear();
			resultsSet_.clear();
		} else if (resultState_.object_ == QueryResult.FALSE) {
			resultState_.object_ = QueryResult.TRUE;
			Substitution s = new Substitution();
			results_.add(s);
			resultsSet_.add(s);
		}
		
		if (goalResult_ == QueryResult.TRUE)
			goalResult_ = QueryResult.FALSE;
		if (goalResult_ == QueryResult.FALSE)
			goalResult_ = QueryResult.TRUE;
	}

	public ErrorEdge getRejectionReason() {
		return rejectionReason_;
	}

	public void setRejectionReason(ErrorEdge errorEdge) {
		rejectionReason_ = errorEdge;
	}

	public void setJustification(List<Node[]> justification) {
		justification_ = justification;
	}

	public boolean isRun() {
		return hasRun_;
	}

	public void setRun(boolean b) {
		hasRun_ = b;
	}

	public boolean shouldVerify() {
		return verifyArguments_;
	}

	public void setVerify(boolean shouldVerify) {
		verifyArguments_ = shouldVerify;
	}

	/**
	 * If the goal of this query desires a particular result.
	 *
	 * @param result
	 *            The result that could be produced.
	 * @return True if the goal allows the result.
	 */
	public boolean desiresResult(QueryResult result) {
		// If all results, or goal is open, return true
		if (goalResult_ == QueryResult.ALL || result == QueryResult.ALL
				|| result == QueryResult.NIL)
			return true;
		// If after true and true, return true
		if (result == QueryResult.TRUE && goalResult_ == QueryResult.TRUE)
			return true;
		// If after false and false, return true
		if (result == QueryResult.FALSE && goalResult_ == QueryResult.FALSE)
			return true;
		return false;
	}

	public QueryResult getGoalResult() {
		return goalResult_;
	}

	public void setGoalResult(QueryResult newGoal) {
		goalResult_ = newGoal;
	}
}
