package graph.inference.module;

import graph.core.CommonConcepts;
import graph.core.Edge;
import graph.core.Node;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;

public class ResultFnWorker extends QueryWorker {
	private static final long serialVersionUID = -8814260693762782159L;

	public ResultFnWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		Collection<Edge> functionResults = relatedModule_.findEdgeByNodes(
				queryObj.getNode(0), queryObj.getNode(1));
		for (Edge e : functionResults) {
			if (e.getNodes()[2].equals(queryObj.getNode(2))) {
				queryObj.addResult(new Substitution(), queryObj.getNodes());
				return;
			}
			Node[] transitiveNodes = { CommonConcepts.GENLS.getNode(dag_),
					e.getNodes()[2], queryObj.getNode(2) };
			QueryObject transitiveSearch = queryObj
					.modifyNodes(transitiveNodes);
			querier_.applyModule(CommonConcepts.GENLS.getNodeName(),
					transitiveSearch);
			if (queryObj.isProof() && queryObj.getResults() != null) {
				queryObj.getJustification().add(e.getNodes());
				queryObj.getJustification().addAll(
						transitiveSearch.getJustification());
				return;
			}
		}
	}
}
