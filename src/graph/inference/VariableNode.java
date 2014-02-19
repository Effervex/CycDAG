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

import graph.core.StringNode;

public class VariableNode extends StringNode {
	public VariableNode(String string) {
		super(string);
	}

	private static final long serialVersionUID = -918899352582359058L;
	public static final VariableNode DEFAULT = new VariableNode("?X");

	@Override
	public String toString() {
		return getName();
	}

}
