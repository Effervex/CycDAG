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

import graph.core.CycDAG;
import graph.core.DirectedAcyclicGraph;
import graph.module.cli.PredsForCommand;
import graph.module.cli.ValidArgCommand;

import java.io.IOException;
import java.net.Socket;

import core.CommandParser;
import core.CommandQueue;
import core.PortHandler;

public class CycDAGCLI extends DAGCommandLineInterface {
	public static final String EDGES_FORWARD_INSTANTIATE = "/env/instantiateEdgeConstraints";

	public CycDAGCLI(int aPort, DirectedAcyclicGraph dag) {
		super(aPort, dag);

		CommandParser.addCommand("randomNode", RandomCycNodeCommand.class);
		CommandParser.addCommand("randomEdge", RandomCycEdgeCommand.class);
		CommandParser.addCommand("extract", ExtractCommand.class);
		CommandParser.addCommand("removeAll", RemoveCommand.class);
		CommandParser.addCommand("validArg", ValidArgCommand.class);
		CommandParser.addCommand("addedge", AddCycEdgeCommand.class);
		CommandParser.addCommand("varhelp", CycDAGVarHelpCommand.class);
		CommandParser.addCommand("predsFor", PredsForCommand.class);
	}

	public static void main(String[] args) {
		try {
			CycDAG dag = new CycDAG(getRootDir(args));
			new CycDAGCLI(getPort(args), dag).start();

			dag.initialise();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	protected PortHandler createPortHandler(Socket serverSocket,
			CommandQueue commandQueue) throws IOException {
		return new CycDAGPortHandler(serverSocket, commandQueue, dag_);
	}
}
