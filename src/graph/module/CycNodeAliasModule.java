package graph.module;

import graph.core.EdgeModifier;
import graph.core.DAGEdge;

public class CycNodeAliasModule extends NodeAliasModule {
	private static final long serialVersionUID = 1L;

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return !EdgeModifier.isSpecial(edge, dag_);
	}
}
