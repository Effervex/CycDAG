package graph.core.cli;

import graph.core.CycDAG;
import graph.core.DirectedAcyclicGraph;
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
		CommandParser.addCommand("validArg", ValidArgCommand.class);
	}

	public static void main(String[] args) {
		CycDAG dag = new CycDAG(getRootDir(args));
		new CycDAGCLI(getPort(args), dag).start();

		dag.initialise();
	}
	
	@Override
	protected PortHandler createPortHandler(Socket serverSocket,
			CommandQueue commandQueue) throws IOException {
		return new CycDAGPortHandler(serverSocket, commandQueue, dag_);
	}
}
