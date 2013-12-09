package graph.inference.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.QueryModule;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.collections4.CollectionUtils;

public class IsaWorker extends QueryWorker {
	private static final long serialVersionUID = 5976403575945727071L;

	public IsaWorker(QueryModule queryModule) {
		super(queryModule);
	}

	protected Collection<Edge> atomicEdges(Node[] nodes, Node atomicNode,
			int atomicIndex, int variableIndex, Collection<Edge> isaPred) {
		Object[] args = new Object[2 * (nodes.length - 2)];
		int a = 0;
		for (int i = 1; i < nodes.length; i++) {
			if (i != variableIndex) {
				if (i == atomicIndex)
					args[a * 2] = atomicNode;
				else
					args[a * 2] = nodes[i];
				args[a * 2 + 1] = i + 1;
				a++;
				if (a * 2 == args.length)
					break;
			}
		}
		Collection<Edge> atomicEdges = relatedModule_.execute(args);
		atomicEdges = CollectionUtils.retainAll(atomicEdges, isaPred);
		return atomicEdges;
	}

	protected QueryObject composeTransitive(DAGNode transitivePred,
			int atomicIndex, DAGNode atomicNode, Node varNode,
			QueryObject queryObj) {
		Node[] transitiveArgs = new Node[3];
		transitiveArgs[0] = transitivePred;
		transitiveArgs[atomicIndex] = atomicNode;
		if (atomicIndex == 1)
			transitiveArgs[2] = varNode;
		else
			transitiveArgs[1] = varNode;

		QueryObject transitiveObj = (queryObj == null) ? new QueryObject(
				transitiveArgs) : queryObj.modifyNodes(transitiveArgs);
		querier_.applyModule(transitivePred.getName(), transitiveObj);
		return transitiveObj;
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		int atomicIndex = queryObj.getAtomicIndex();
		int varIndex = (atomicIndex == 1) ? 2 : 1;
		DAGNode atomic = queryObj.getAtomic();
		if (atomic == null)
			return;

		VariableNode varNode = new VariableNode("?_T_");
		// Find downwards transitive if atomic is not first arg.
		QueryObject transitiveQO = null;
		if (atomicIndex != 1)
			transitiveQO = composeTransitive(
					CommonConcepts.GENLS.getNode(dag_), atomicIndex, atomic,
					varNode, null);

		Collection<Substitution> genlsSubs = new ArrayList<>(1);
		if (transitiveQO != null)
			genlsSubs = transitiveQO.getResults();
		if (genlsSubs.isEmpty())
			genlsSubs.add(new Substitution(varNode, atomic));

		Collection<Edge> isaPred = relatedModule_.findEdgeByNodes(queryObj
				.getNode(0));

		for (Substitution s : genlsSubs) {
			Node n = s.getSubstitution(varNode);
			Collection<Edge> isas = atomicEdges(queryObj.getNodes(), n,
					atomicIndex, varIndex, isaPred);

			// Checking functions
			if (atomicIndex == 1 && n instanceof OntologyFunction) {
				Collection<DAGNode> functionEdges = querier_.functionResults(
						(OntologyFunction) atomic, CommonConcepts.RESULT_ISA);
				for (DAGNode funcNode : functionEdges)
					isas.add(new DAGEdge(CommonConcepts.ISA.getNode(dag_), n,
							funcNode));
			}
			for (Edge e : isas) {
				Node[] edgeNodes = e.getNodes();
				if (queryObj.isCompleted(edgeNodes[varIndex]))
					continue;
				queryObj.getJustification().clear();
				if (queryObj.addResult(edgeNodes))
					return;

				// Find upwards transitive if atomic is first arg.
				if (atomicIndex == 1 && edgeNodes[varIndex] instanceof DAGNode) {
					transitiveQO = composeTransitive(
							CommonConcepts.GENLS.getNode(dag_), atomicIndex,
							(DAGNode) edgeNodes[varIndex],
							queryObj.getNode(varIndex), queryObj);

					// If the proof is met, exit
					if (queryObj != null && queryObj.isProof()
							&& queryObj.getResults() != null) {
						queryObj.getJustification().addAll(
								transitiveQO.getJustification());
						return;
					}
				}
			}
		}

		if (atomicIndex != 1) {
			transitiveQO.cleanTransitiveJustification(transitiveQO
					.getJustification());
			queryObj.getJustification().addAll(transitiveQO.getJustification());
		}
	}

	@Override
	public void setDAG(DirectedAcyclicGraph dag) {
		super.setDAG(dag);
	}
}
