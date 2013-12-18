/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand
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
