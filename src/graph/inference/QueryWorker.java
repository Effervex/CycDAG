package graph.inference;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.module.ModuleException;
import graph.module.OntologyEdgeModule;
import graph.module.QueryModule;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

public abstract class QueryWorker implements Serializable {
	private static final long serialVersionUID = -9204653084421990460L;

	protected QueryModule querier_;

	protected transient OntologyEdgeModule relatedModule_;

	protected transient DirectedAcyclicGraph dag_;

	public QueryWorker(QueryModule queryModule) {
		querier_ = queryModule;
	}

	public Collection<Substitution> query(Node... nodes)
			throws IllegalArgumentException {
		QueryObject queryObj = new QueryObject(nodes);
		queryInternal(queryObj);
		return queryObj.getResults();
	}

	/**
	 * Returns all nodes that a function produces (either resultGenl or
	 * resultIsa, and their expanded <X>Arg variants). Other methods can use
	 * this in their calculations of a query result.
	 * 
	 * @param functionNode
	 *            The function to return nodes for.
	 * @param resultQuery
	 *            The type of results to look for (resultGenl/resultIsa).
	 * @return The collection of all results returned by the function.
	 */
	public Collection<DAGNode> functionResults(OntologyFunction functionNode,
			CommonConcepts resultQuery) {
		Collection<DAGNode> results = new ArrayList<>();
		Collection<Edge> resultEdges = relatedModule_.findEdgeByNodes(
				resultQuery.getNode(dag_), functionNode.getNodes()[0]);
		for (Edge e : resultEdges)
			results.add((DAGNode) e.getNodes()[2]);

		// resultArgs
		CommonConcepts resultArgConcept = (resultQuery == CommonConcepts.RESULT_GENL) ? CommonConcepts.RESULT_GENL_ARG
				: (resultQuery == CommonConcepts.RESULT_ISA) ? CommonConcepts.RESULT_ISA_ARG
						: null;
		resultEdges = relatedModule_.findEdgeByNodes(resultArgConcept.getNode(dag_),
				functionNode.getNodes()[0]);
		for (Edge e : resultEdges) {
			Integer argIndex = Integer.parseInt(e.getNodes()[2].toString());
			Node n = functionNode.getNodes()[argIndex];
			if (n instanceof DAGNode)
				results.add((DAGNode) n);
		}

		return results;
	}

	/**
	 * Asks a query containing one or more variables and returns all possible
	 * substitutions for the variables.
	 * 
	 * @param queryObj
	 *            The necessary information and returned object for the query.
	 */
	public abstract void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException;

	public void setDAG(DirectedAcyclicGraph dag) {
		dag_ = dag;
		relatedModule_ = (OntologyEdgeModule) dag
				.getModule(OntologyEdgeModule.class);
		if (relatedModule_ == null)
			throw new ModuleException(
					"Cannot perform query without related edge module.");
	}
}
