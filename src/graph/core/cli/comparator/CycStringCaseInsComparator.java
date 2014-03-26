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
package graph.core.cli.comparator;

import graph.core.cli.comparator.StringCaseInsComparator;

public class CycStringCaseInsComparator extends StringCaseInsComparator {
	@Override
	protected int compareInternal(Object o1, Object o2) {
		String o1Str = CycStringComparator.processString(o1);
		String o2Str = CycStringComparator.processString(o2);
		return super.compareInternal(o1Str, o2Str);
	}
}
