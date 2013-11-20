package graph.module;

import graph.core.DAGNode;
import graph.core.Edge;

import java.util.Collection;

import util.MultiMap;

// TODO Make this class an index of node depths for quick access to certain levels of node depth.
public class DepthModule extends DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 7586206693104940128L;

	private MultiMap<Integer, DAGNode> depthMap_;

	public DepthModule() {
		depthMap_ = MultiMap.createConcurrentHashSetMultiMap();
	}

	@Override
	public boolean addEdge(Edge edge) {
		return super.addEdge(edge);
	}

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		// TODO Auto-generated method stub
		return null;
	}
}
