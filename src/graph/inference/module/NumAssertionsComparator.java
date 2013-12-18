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
package graph.inference.module;

import graph.core.CommonConcepts;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.inference.VariableNode;
import graph.module.RelatedEdgeModule;

import java.util.Comparator;

public class NumAssertionsComparator implements Comparator<Edge> {
	private RelatedEdgeModule relatedEdgeModule_;
	private DirectedAcyclicGraph dag_;

	public NumAssertionsComparator(RelatedEdgeModule relatedModule,
			DirectedAcyclicGraph dag) {
		relatedEdgeModule_ = relatedModule;
		dag_ = dag;
	}

	@Override
	public int compare(Edge o1, Edge o2) {
		int numVars1 = 0;
		int numVars2 = 0;
		int count1 = 0;
		int count2 = 0;
		Node[] nodes1 = o1.getNodes();
		Node[] nodes2 = o2.getNodes();

		// Different/Equals always goes last
		int diffResult = 0;
		if (nodes1[0].equals(CommonConcepts.DIFFERENT.getNode(dag_))
				|| nodes1[0].equals(CommonConcepts.EQUALS.getNode(dag_)))
			diffResult += 1;
		if (nodes2[0].equals(CommonConcepts.DIFFERENT.getNode(dag_))
				|| nodes2[0].equals(CommonConcepts.EQUALS.getNode(dag_)))
			diffResult -= 1;
		if (diffResult != 0)
			return diffResult;

		for (int i = 0; i < Math.max(nodes1.length, nodes2.length); i++) {
			Node n1 = (i < nodes1.length) ? nodes1[i] : null;
			Node n2 = (i < nodes2.length) ? nodes2[i] : null;
			// No need to check if equal
			if (n1 != null && n2 != null && n1.equals(n2))
				continue;

			if (n1 != null) {
				if (n1 instanceof VariableNode)
					numVars1++;
				else
					count1 += relatedEdgeModule_.execute(nodes1[i], i + 1)
							.size();
			}
			if (n2 != null) {
				if (n2 instanceof VariableNode)
					numVars2++;
				else
					count2 += relatedEdgeModule_.execute(nodes2[i], i + 1)
							.size();
			}

			if (n1 == null && count2 > count1)
				return -1;
			if (n2 == null && count1 > count2)
				return 1;
		}

		int result = Integer.compare(numVars1, numVars2);
		if (result != 0)
			return result;
		return Integer.compare(count1, count2);
	}

}
