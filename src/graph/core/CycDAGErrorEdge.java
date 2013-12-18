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

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public enum CycDAGErrorEdge implements ErrorEdge {
	VARIABLE_NODE("Edge using a variable node."), DISJOINT_EDGE(
			"Edge is disjoint with existing information."), SEMANTIC_CONFLICT(
			"Edge using semantically invalid arguments.");

	private String error_;

	private CycDAGErrorEdge(String error) {
		error_ = error;
	}

	@Override
	public String getIdentifier() {
		return toString();
	}

	@Override
	public String getError() {
		return error_;
	}

	@Override
	public boolean containsNode(Node node) {
		return false;
	}

	@Override
	public Node[] getNodes() {
		return null;
	}

	@Override
	public String toString(boolean useIDs) {
		return toString();
	}

	@Override
	public long getID() {
		return -1;
	}

	public void readExternal(ObjectInput in) throws IOException,
			ClassNotFoundException {
		error_ = (String) in.readObject();
	}

	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeObject(error_);
	}
}
