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

public enum PropagatablePredicate {
	ISA(CommonConcepts.ISA),
	GENLS(CommonConcepts.GENLS),
	DISJOINTWITH(CommonConcepts.DISJOINTWITH),
	GENLMT(CommonConcepts.GENLMT),
	GENLINVERSE(CommonConcepts.GENLINVERSE);

	private CommonConcepts pred_;

	private PropagatablePredicate(CommonConcepts pred) {
		pred_ = pred;
	}

	public CommonConcepts getPred() {
		return pred_;
	}
}
