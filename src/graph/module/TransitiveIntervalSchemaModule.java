package graph.module;

import graph.core.DAGEdge;
import graph.core.DAGNode;

import java.util.Collection;

public class TransitiveIntervalSchemaModule extends
		DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 6562719667555853873L;

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public void initialisationComplete(Collection<DAGNode> nodes, Collection<DAGEdge> edges) {
		
	}
}
