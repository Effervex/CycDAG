package graph.inference.module;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Queue;

import org.apache.commons.collections4.CollectionUtils;

public class TransitiveWorker extends QueryWorker {
	private static final long serialVersionUID = -7763389018408199476L;

	public TransitiveWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		transitiveSearch(queryObj);
		if (queryObj.isProof())
			queryObj.cleanTransitiveJustification(queryObj.getJustification());
	}

	protected void transitiveSearch(QueryObject queryObj) {
		// Find the index
		int atomicIndex = queryObj.getAtomicIndex();
		int varIndex = (atomicIndex == 1) ? 2 : 1;
		DAGNode atomic = queryObj.getAtomic();
		if (atomic == null)
			return;

		Queue<DAGNode> toCheck = new LinkedList<>();
		toCheck.add(atomic);
		Collection<Edge> genlEdges = relatedModule_
				.findEdgeByNodes((DAGNode) queryObj.getNode(0));

		while (!toCheck.isEmpty()) {
			DAGNode n = querier_.getExpanded(toCheck.poll());
			if (atomicIndex == 1 && n instanceof OntologyFunction) {
				if (querier_.functionResult((OntologyFunction) n, varIndex,
						CommonConcepts.RESULT_GENL, queryObj))
					return;
			}

			if (queryObj.isCompleted(n))
				continue;
			queryObj.addCompleted(n);

			// Intersect the collections
			Collection<Edge> nodeEdges = relatedModule_.execute(n,
					atomicIndex + 1);
			if (nodeEdges == null || genlEdges == null)
				continue;
			nodeEdges = CollectionUtils.retainAll(nodeEdges, genlEdges);

			// Self genls check
			if (n == atomic) {
				Collection<Edge> selfEdges = nodeEdges;
				if (selfEdges.isEmpty()) {
					selfEdges = relatedModule_.execute(atomic, varIndex + 1);
					selfEdges = CollectionUtils.retainAll(selfEdges, genlEdges);
				}
				if (!selfEdges.isEmpty()) {
					if (queryObj.addResult(queryObj.getNode(0), atomic, n))
						return;
				}
			}

			// Create the subs
			for (Edge e : nodeEdges) {
				try {
					DAGNode edgeNode = (DAGNode) e.getNodes()[varIndex];

					if (queryObj.isProof()
							&& e.getNodes()[varIndex].equals(queryObj
									.getNode(2))) {
						queryObj.addResult(new Substitution(), e.getNodes());
						return;
					}
					if (queryObj.addResult(e.getNodes()))
						return;
					toCheck.add(edgeNode);
				} catch (ClassCastException cce) {
					System.err.print(e);
					cce.printStackTrace();
				}
			}
		}
	}
}
