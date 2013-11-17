package graph.module;

import graph.core.DAGNode;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.util.Set;

import util.Trie;

public class FunctionIndex extends DAGModule<OntologyFunction> {
	private static final long serialVersionUID = -2107172826877208005L;

	private Trie<Node, OntologyFunction> index_;

	public FunctionIndex() {
		super();
		index_ = new Trie<>();
	}

	@Override
	public boolean addNode(DAGNode node) {
		if (node instanceof OntologyFunction) {
			return index_.put(((OntologyFunction) node).getNodes(), 0,
					(OntologyFunction) node);
		}
		return true;
	}

	@Override
	public OntologyFunction execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		Set<OntologyFunction> vals = index_.getValue((Node[]) args, 0);
		if (vals == null || vals.isEmpty())
			return null;
		OntologyFunction func = vals.iterator().next();
		if (idModule_ && func != null)
			return (OntologyFunction) dag_.getNodeByID(func.getID());
		else
			return func;
	}

	@Override
	public String toString() {
		return index_.toString();
	}
}
