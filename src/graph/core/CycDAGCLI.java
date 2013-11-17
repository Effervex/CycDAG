package graph.core;

import graph.core.cli.DAGCommandLineInterface;
import graph.module.cli.RandomCycEdgeCommand;
import graph.module.cli.RandomCycNodeCommand;
import core.CommandParser;

public class CycDAGCLI extends DAGCommandLineInterface {
	public static final String EDGES_FORWARD_INSTANTIATE = "/env/instantiateEdgeConstraints";

	public CycDAGCLI(int aPort, DirectedAcyclicGraph dag) {
		super(aPort, dag);

		CommandParser.addCommand("randomNode", RandomCycNodeCommand.class);
		CommandParser.addCommand("randomEdge", RandomCycEdgeCommand.class);
	}

	public static void main(String[] args) {
		CycDAG dag = new CycDAG(getRootDir(args));
		new CycDAGCLI(getPort(args), dag).start();

		dag.initialise(args);
	}
}
