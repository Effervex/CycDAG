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
package graph.core;

import graph.module.QueryModule;

import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;

public class OntologyFunction extends DAGNode implements Edge {
	private static final long serialVersionUID = 473544398260462641L;
	private boolean notAnonymous_ = true;
	protected Node[] nodes_;

	public OntologyFunction() {
		super();
	}

	public OntologyFunction(Node... nodes) {
		super();
		nodes_ = nodes;
		notAnonymous_ = false;
		id_ = requestID();
	}

	protected OntologyFunction(DirectedAcyclicGraph dag, Node... nodes) {
		super();
		nodes_ = nodes;
		notAnonymous_ = !checkIfAnonymous(dag);
		id_ = requestID();
	}

	@Override
	protected int requestID() {
		// Default to -1, unless not anonymous
		if (notAnonymous_)
			return super.requestID();
		return -1;
	}

	private boolean checkIfAnonymous(DirectedAcyclicGraph dag) {
		// Check if function is unreifiable
		QueryModule queryModule = (QueryModule) dag
				.getModule(QueryModule.class);
		// If nodes[0] is unreifiable OR is not a function
		if (queryModule.prove(CommonConcepts.ISA.getNode(dag), nodes_[0],
				CommonConcepts.UNREIFIABLE_FUNCTION.getNode(dag)))
			return true;
		// TODO Need to put this back in. But can't for now due to DAGrecreate
		// loading.
		// if (!queryModule.prove(CommonConcepts.ISA.getNode(dag), nodes_[0],
		// CommonConcepts.FUNCTION.getNode(dag)))
		// return true;
		return false;
	}

	@Override
	public int compareTo(DAGObject o) {
		int result = super.compareTo(o);
		if (result != 0)
			return result;

		return toString().compareTo(o.toString());
	}

	@Override
	public boolean containsNode(Node node) {
		for (Node edgeNode : getNodes())
			if (edgeNode.equals(node))
				return true;
		return false;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		OntologyFunction other = (OntologyFunction) obj;
		if (!Arrays.equals(nodes_, other.nodes_))
			return false;
		return true;
	}

	@Override
	public String getIdentifier() {
		return getIdentifier(false);
	}
	
	@Override
	public String getIdentifier(boolean useName) {
		if (id_ != -1 && !useName)
			return id_ + "";

		StringBuffer buffer = new StringBuffer("(");
		boolean first = true;
		for (Node n : nodes_) {
			if (!first)
				buffer.append(" ");
			buffer.append(n.getIdentifier(useName));
			first = false;
		}
		buffer.append(")");
		return buffer.toString();
	}

	@Override
	public String getName() {
		return "(" + StringUtils.join(nodes_, ' ') + ")";
	}

	@Override
	public Node[] getNodes() {
		return nodes_;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = prime + Arrays.hashCode(nodes_);
		return result;
	}

	@Override
	public boolean isAnonymous() {
		return !notAnonymous_;
	}

	@Override
	public String toString(boolean useIDs) {
		if (!useIDs)
			return toString();
		StringBuffer buffer = new StringBuffer("(");
		boolean first = true;
		for (Node n : nodes_) {
			if (!first)
				buffer.append(" ");
			buffer.append(n.getIdentifier());
			first = false;
		}
		buffer.append(")");
		return buffer.toString();
	}
}
