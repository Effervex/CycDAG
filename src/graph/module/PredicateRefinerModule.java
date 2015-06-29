package graph.module;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.EdgeModifier;
import graph.core.Node;
import graph.core.PrimitiveNode;
import graph.core.StringNode;
import graph.inference.CommonQuery;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

public class PredicateRefinerModule extends DAGModule<Boolean> {
	private static final double CONSTRAINT_THRESHOLD = 0.95; // TODO Arbitrary
	/** The minimum amount fo evidence needed to begin refinement. */
	private static final int MIN_EVIDENCE = 100; // TODO Arbitrary
	private static final StringNode PREDICATE_REFINER_CREATOR = new StringNode(
			"PredicateRefinerModule");
	private static final long serialVersionUID = 1L;

	private RelatedEdgeModule relEdgeModule_;

	public PredicateRefinerModule() {
		super();
	}

	/**
	 * Checks if the threshold is possible for the current count vector, given
	 * the total and num processed.
	 *
	 * @param count
	 *            The current count.
	 * @param numProcessed
	 *            The number processed.
	 * @param numTotal
	 *            The total number to process.
	 * @param threshold
	 *            The threshold to meet.
	 * @return True if the threshold is still theoretically attainable.
	 */
	private boolean isThresholdAttainable(Integer count, int numProcessed,
			int numTotal, double threshold) {
		int maxFails = (int) Math.floor(numTotal * (1 - threshold));
		if (numProcessed - count > maxFails)
			return false;
		return true;
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		// TODO Maybe move to an incremental system
		return super.addEdge(edge);
	}

	/**
	 * Begins the predicate refinement operation, searching for
	 * ReinfinablePedicates with an
	 */
	@Override
	public Boolean execute(Object... args) throws IllegalArgumentException,
			ModuleException {// Identify Refinable Predicates
		Collection<Node> refinables = CommonQuery.DIRECTINSTANCE.runQuery(dag_,
				CommonConcepts.REFINABLE_PREDICATE.getNode(dag_));
		if (refinables.isEmpty())
			return false;

		// For each refinable predicate, process is sufficient evidence
		for (Node refPred : refinables) {
			Collection<Edge> refEdges = relEdgeModule_.execute(refPred, 1);
			if (refEdges.size() >= MIN_EVIDENCE) {
				// Enough evidence to begin inference
				inferConstraints(refPred, refEdges, CONSTRAINT_THRESHOLD);
			}
		}
		return true;
	}

	/**
	 * The primary method for inferring argument constraints from edge data. The
	 * algorithm records counts for every isa of every edge, removing possible
	 * collections if the upper limit falls below the threshold.
	 *
	 * @param refPred
	 *            The refined predicate to infer constraints for.
	 * @param refEdges
	 *            The refined edges to infer constraints from.
	 * @param ontology
	 *            The ontology access.
	 */
	public void inferConstraints(Node refPred, Collection<Edge> refEdges,
			double threshold) {
		Map[] counts = { new HashMap<Node, Integer>(),
				new HashMap<Node, Integer>() };
		int numEdges = refEdges.size();
		int minCount = (int) Math.ceil(numEdges * threshold);
		int edgeIndex = 0;
		for (Edge edge : refEdges) {
			edgeIndex++;
			// For every argument
			Node[] args = ((DAGEdge) edge).getNodes();
			for (int i = 1; i < args.length; i++) {
				// Get all isas
				Collection<Node> isas = CommonQuery.ALLISA.runQuery(dag_,
						args[i]);

				// Record counts, ignoring -1 counts
				Map<Node, Integer> countMap = counts[i - 1];
				for (Node isa : isas) {
					if (!(isa instanceof DAGNode))
						continue;
					int id = ((DAGNode) isa).getID();
					if (id == -1)
						continue;

					// Update count
					Integer count = countMap.get(isa);
					if (count == null)
						count = 0;
					else if (count == -1)
						continue;
					count++;

					// If count falls below possible threshold, set count as -1
					if (isThresholdAttainable(count, edgeIndex, numEdges,
							threshold))
						countMap.put(isa, count);
					else
						countMap.put(isa, -1);
				}
			}
		}

		// Find min genls among all possibilities
		boolean refined = false;
		for (int i = 0; i < counts.length; i++) {
			Collection<Node> possible = new ArrayList<>();
			for (Object count : counts[i].entrySet()) {
				Map.Entry<Node, Integer> entry = (Entry<Node, Integer>) count;
				if (entry.getValue() >= minCount)
					possible.add(entry.getKey());
			}
			Collection<? extends Node> minGenls = CommonQuery.minGeneralFilter(
					possible, dag_);

			// Set constraint as min genls (as isa)
			for (Node minGenl : minGenls) {
				((CycDAG) dag_).findOrCreateEdge(new Node[] {
						CommonConcepts.ARGISA.getNode(dag_), refPred,
						PrimitiveNode.parseNode((i + 1) + ""), minGenl },
						PREDICATE_REFINER_CREATOR, true, false);
				refined = true;
			}
		}

		// Remove refined
		if (refined) {
			Edge refinedEdge = dag_.findEdge(CommonConcepts.ISA.getNode(dag_),
					refPred, CommonConcepts.REFINABLE_PREDICATE.getNode(dag_));
			((CycDAG) dag_).removeEdge(refinedEdge, true);
		}
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		relEdgeModule_ = (RelatedEdgeModule) directedAcyclicGraph
				.getModule(RelatedEdgeModule.class);
		super.setDAG(directedAcyclicGraph);
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return !EdgeModifier.isRemoved(edge, dag_);
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}
}
