package graph.core.cli;

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.cli.comparator.DefaultComparator;
import graph.core.cli.comparator.DepthComparator;
import graph.inference.Substitution;

import java.net.Socket;
import java.util.Map;

import core.CommandQueue;

public class CycDAGPortHandler extends DAGPortHandler {
	public CycDAGPortHandler(Socket aSocket, CommandQueue aQueue,
			DirectedAcyclicGraph dag) {
		super(aSocket, aQueue, dag);
	}

	@Override
	protected DefaultComparator getComparator() {
		DefaultComparator comparator = super.getComparator();
		if (comparator != null)
			return comparator;
		if (get(SORT_ORDER).equals("depth")) {
			comparator = new DepthComparator();
		}
		return comparator;
	}

	@Override
	public Object convertToComparable(Object o) {
		if (o instanceof Substitution) {
			Map<String, Node> subMap = ((Substitution) o).getSubstitutionMap();
			return subMap.get(subMap.keySet().iterator().next());
		}
		return super.convertToComparable(o);
	}
}
