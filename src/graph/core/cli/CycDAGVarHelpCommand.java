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
