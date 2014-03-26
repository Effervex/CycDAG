/*******************************************************************************
 * Copyright (c) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.core;

public class VariableErrorEdge extends DAGErrorEdge {
	private static final long serialVersionUID = 1L;
	private static VariableErrorEdge instance_ = new VariableErrorEdge();

	@Override
	public String getError() {
		return "Variable(s) found in assertion.";
	}

	@Override
	public Node[] getNodes() {
		return null;
	}

	public static VariableErrorEdge getInstance() {
		return instance_;
	}

}
