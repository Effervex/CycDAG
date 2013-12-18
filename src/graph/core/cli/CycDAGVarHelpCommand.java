/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand
 ******************************************************************************/
package graph.core.cli;

public class CycDAGVarHelpCommand extends DAGVarHelpCommand {
	@Override
	protected String getVarHelp(String variable) {
		if (variable.equals(DAGPortHandler.SORT_ORDER))
			return "[STRING] Enforces a sort order on certain command "
					+ "outputs (using DAGPortHandler's 'sort'). The "
					+ "different sort orders include: alpha, for "
					+ "alphabetical; alphaNoCase, for alphabetical "
					+ "(caseless); id, for ID ordered results; depth, "
					+ "for depth (from Thing) ordered results.";
		return super.getVarHelp(variable);
	}
}
