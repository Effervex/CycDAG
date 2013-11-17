package graph.inference.module;

import graph.core.Node;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.HashSet;
import java.util.Set;

public class DifferentWorker extends QueryWorker {

	public DifferentWorker(QueryModule queryModule) {
		super(queryModule);
	}

	/**  */
	private static final long serialVersionUID = 3330771248174687936L;

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		Node[] nodes = queryObj.getNodes();
		Set<Node> nodeEquals = new HashSet<>();
		for (int i = 1; i < nodes.length; i++) {
			if (!nodeEquals.add(nodes[i]))
				return;
		}
		queryObj.addResult(new Substitution(), nodes);
	}
}
