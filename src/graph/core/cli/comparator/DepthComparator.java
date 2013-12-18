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
package graph.core.cli.comparator;

import graph.core.DAGNode;
import graph.module.DepthModule;

public class DepthComparator extends IDComparator {

	@Override
	protected int compareInternal(Object o1, Object o2) {
		if (o1 instanceof DAGNode && o2 instanceof DAGNode) {
			String strDepth1 = ((DAGNode) o1)
					.getProperty(DepthModule.DEPTH_PROPERTY);
			String strDepth2 = ((DAGNode) o2)
					.getProperty(DepthModule.DEPTH_PROPERTY);
			if (strDepth1 != null) {
				if (strDepth2 != null) {
					int depth1 = Integer.parseInt(strDepth1);
					int depth2 = Integer.parseInt(strDepth2);
					if (depth1 != depth2)
						return Integer.compare(depth1, depth2);
				} else
					return -1;
			} else if (strDepth2 != null)
				return 1;
		}
		return super.compareInternal(o1, o2);
	}

}
