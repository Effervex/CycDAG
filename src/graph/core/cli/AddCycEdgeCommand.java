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
package graph.core.cli;

import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;

import java.util.ArrayList;

import org.apache.commons.lang3.StringUtils;

import util.UtilityMethods;

public class AddCycEdgeCommand extends AddEdgeCommand {
	@Override
	public String helpText() {
		return "{0} (node node ...)[:MT] [(creator)] : Creates an edge "
				+ "composed of two or more nodes, optionally in a particular "
				+ "microtheory and with an optional creator.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		ArrayList<String> split = UtilityMethods.split(data, ' ');

		// Extract microtheory (if one exists)
		ArrayList<String> edgeMt = UtilityMethods.split(split.get(0), ':');
		String edgeStr = edgeMt.get(0);
		String microtheory = (edgeMt.size() > 1) ? StringUtils.join(
				edgeMt.subList(1, edgeMt.size()), ':') : null;

		Node creator = null;
		if (split.size() > 1) {
			try {
				creator = dagHandler.getDAG().findOrCreateNode(
						UtilityMethods.shrinkString(split.get(1), 1), creator);
			} catch (Exception e) {
				print("-1|Invalid creator node.\n");
				return;
			}
		}

		try {
			Node[] nodes = dagHandler.getDAG().parseNodes(
					edgeStr,
					creator,
					dagHandler.get(DAGPortHandler.DYNAMICALLY_ADD_NODES)
							.equals("true"), false);
			if (nodes == null) {
				print("-1|Problem parsing nodes.\n");
				return;
			}
			boolean[] flags = dagHandler
					.asBooleanArray(DAGPortHandler.EDGE_FLAGS);
			if (flags.length < 1)
				flags = new boolean[1];
			flags[0] = true;
			Edge edge = ((CycDAG) dagHandler.getDAG()).findOrCreateEdge(
					nodes, creator, microtheory, flags);
			dagHandler.getDAG().writeCommand("addedge " + data);

			if (edge instanceof ErrorEdge) {
				print("-1|" + ((ErrorEdge) edge).getError() + "\n");
			} else {
				DAGEdge dagEdge = (DAGEdge) edge;
				print(dagEdge.getID()
						+ "|"
						+ edge.toString(!dagHandler.get(
								DAGPortHandler.PRETTY_RESULTS).equals("true"))
						+ "|" + dagEdge.getCreator() + "|"
						+ dagEdge.getCreationDate() + "\n");
			}
		} catch (Exception e) {
			e.printStackTrace();
			print("-1|Problem parsing nodes.\n");
			return;
		}
	}
}
