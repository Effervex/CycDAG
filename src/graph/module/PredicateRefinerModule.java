package graph.module;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.EdgeModifier;
import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.PrimitiveNode;
import graph.core.StringNode;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.QueryResult;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;

public class PredicateRefinerModule extends DAGModule<Collection<DAGNode>> {
	private static final StringNode PREDICATE_REFINER_CREATOR = new StringNode(
			"PredicateRefinerModule");
	private static final long serialVersionUID = 1L;

	/** The counts for each node. */
	private transient Map<Node, NodeDetails> counts_;

	/** The global details to set maximally general constraints. */
	private transient NodeDetails globalCounts_;

	/** The query module access. */
	private transient QueryModule querier_;

	/** Related edge module access. */
	private transient RelatedEdgeModule relEdgeModule_;

	public PredicateRefinerModule() {
		super();
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		// TODO Maybe move to an incremental system
		return super.addEdge(edge);
	}

	/**
	 * Begins the predicate refinement operation, searching for
	 * ReinfinablePedicates with a minimum number of assertions as evidence and
	 * a threshold of common evidence.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		counts_ = new HashMap<>();
		globalCounts_ = new NodeDetails();

		// Identify Refinable Predicates
		Integer minEvidence = (Integer) args[0];
		Double threshold = (Double) args[1];
		Collection<Node> refinables = CommonQuery.DIRECTINSTANCE.runQuery(dag_,
				CommonConcepts.REFINABLE_PREDICATE.getNode(dag_));
		if (refinables.isEmpty())
			return CollectionUtils.EMPTY_COLLECTION;

		// For each refinable predicate, process is sufficient evidence
		for (Node refPred : refinables) {
			Collection<Edge> refEdges = relEdgeModule_.execute(refPred, 1);
			recordEvidence(refPred, refEdges);
		}
		Collection<DAGNode> refined = inferConstraints(threshold, minEvidence);

		// Impose the constraints
		imposeConstraints(refined);

		return refined;
	}

	/**
	 * Imposes the constraints found for each refined predicate such that any
	 * edges not satisfying the constraints either have their arguments coerced
	 * into the correct collections, or removed.
	 *
	 * @param refined
	 *            The refined predicates to impose constraints for.
	 */
	public void imposeConstraints(Collection<DAGNode> refined) {
		for (DAGNode predicate : refined) {
			Collection<Edge> refEdges = relEdgeModule_.execute(predicate, 1);
			for (Edge e : refEdges) {
				// Attempt to add the edge coercing arguments if necessary
				Edge testEdge = dag_.findOrCreateEdge(e.getNodes(), null, true,
						false, true);
				if (testEdge instanceof ErrorEdge) {
					// If the coercion failed, remove the edge
					dag_.removeEdge(e);
				}
			}
		}
	}

	/**
	 * Infers constraints from the node counts, ignoring constraints that are
	 * equal to the global constraints.
	 *
	 * @param threshold
	 *            The minimum threshold required for counts.
	 * @param minEvidence
	 * @return A collection of all nodes receiving constraints.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private Collection<DAGNode> inferConstraints(double threshold,
			int minEvidence) {
		// Calculate global constraints first
		Collection[] globalConstraints = {
				globalCounts_.inferConstraints(threshold, 1),
				globalCounts_.inferConstraints(threshold, 2) };

		Collection<DAGNode> refinedPreds = new ArrayList<>();
		// For every evidence node, infer constraints
		for (Map.Entry<Node, NodeDetails> entry : counts_.entrySet()) {
			Node refPred = entry.getKey();
			NodeDetails details = entry.getValue();
			boolean refined = false;
			// For each argument
			for (int i = 1; i <= 2; i++) {
				if (details.numEvidence_[i - 1] < minEvidence)
					continue;
				Collection<Node> constraints = details.inferConstraints(
						threshold, i);
				// Only create constraints not in the global set.
				for (Node constraint : constraints) {
					if (!isGeneralConstraint(constraint,
							globalConstraints[i - 1])) {
						((CycDAG) dag_).findOrCreateEdge(new Node[] {
								CommonConcepts.ARGISA.getNode(dag_), refPred,
								PrimitiveNode.parseNode(i + ""), constraint },
								PREDICATE_REFINER_CREATOR, true, false);
						refined = true;
					}
				}
			}

			if (refined)
				refinedPreds.add((DAGNode) refPred);
		}
		return refinedPreds;
	}

	/**
	 * Checks if a constraint is a child of one of the global constraints
	 * defining the general maximum.
	 *
	 * @param constraint
	 *            The constraint to check.
	 * @param globalConstraints
	 *            The global constraints to compare against.
	 * @return True if the constraint is a super collection of a global
	 *         constraint.
	 */
	private boolean isGeneralConstraint(Node constraint,
			Collection<Node> globalConstraints) {
		if (globalConstraints.contains(constraint))
			return true;

		// Check genls
		for (Node global : globalConstraints) {
			QueryObject qo = new QueryObject(false, false, QueryResult.TRUE,
					CommonConcepts.GENLS.getNode(dag_), global, constraint);
			if (querier_.prove(qo) == QueryResult.TRUE)
				return true;
		}
		return false;
	}

	/**
	 * Records the evidence for a RefinableNode, counting the isa evidence to be
	 * used later for inferring constraints.
	 *
	 * @param refPred
	 * @param refEdges
	 */
	public void recordEvidence(Node refPred, Collection<Edge> refEdges) {
		NodeDetails refCounts = new NodeDetails();
		counts_.put(refPred, refCounts);

		// Record each edge's evidence
		for (Edge edge : refEdges) {
			// For every argument
			Node[] args = ((DAGEdge) edge).getNodes();
			for (int i = 1; i < args.length; i++) {
				refCounts.recordCount(args[i], i, null);
			}
		}
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		querier_ = (QueryModule) directedAcyclicGraph
				.getModule(QueryModule.class);
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

	/**
	 * A helper class for tracking counts for a node.
	 *
	 * @author Sam Sarjant
	 */
	private class NodeDetails {
		/** Isa counts from the evidence. */
		private Map<Node, Integer>[] isaArgCounts_;

		/** Keeps track of which evidence arguments have already been counted. */
		private Set<Node>[] processedArguments_;

		/** Evidence counts for each argument of the node. */
		private int[] numEvidence_;

		@SuppressWarnings("unchecked")
		public NodeDetails() {
			isaArgCounts_ = new Map[] { new HashMap<Node, Integer>(),
					new HashMap<Node, Integer>() };
			processedArguments_ = new Set[] { new HashSet<Node>(),
					new HashSet<Node>() };
			numEvidence_ = new int[2];
		}

		/**
		 * Infers the constraints from the counts, using the given threshold.
		 * 
		 * @param threshold
		 *            The threshold to calculate for.
		 * @param argIndex
		 *            The arg index to calculate for.
		 *
		 * @return The set of all constraints for the given index.
		 */
		@SuppressWarnings("unchecked")
		public Collection<Node> inferConstraints(double threshold, int argIndex) {
			int minCount = (int) Math.ceil(threshold
					* numEvidence_[argIndex - 1]);
			Collection<Node> possible = new ArrayList<>();
			if (isaArgCounts_[argIndex - 1] == null)
				return possible;
			for (Object count : isaArgCounts_[argIndex - 1].entrySet()) {
				Map.Entry<Node, Integer> entry = (Entry<Node, Integer>) count;
				if (entry.getValue() >= minCount)
					possible.add(entry.getKey());
			}
			Collection<? extends Node> minGenls = CommonQuery.minGeneralFilter(
					possible, dag_);
			return (Collection<Node>) minGenls;
		}

		/**
		 * Records a count, creating new maps if necessary.
		 *
		 * @param isaNode
		 *            The node to record a count for.
		 * @param argIndex
		 *            The index to record under.
		 */
		public void recordCount(Node edgeNode, int argIndex,
				Collection<Node> isas) {
			if (!processedArguments_[argIndex - 1].add(edgeNode))
				return;

			// Get all isas
			if (isas == null)
				isas = CommonQuery.ALLISA.runQuery(dag_,
						querier_.getExpanded(edgeNode));
			if (isas.isEmpty())
				return;

			numEvidence_[argIndex - 1]++;

			// Record counts
			for (Node isa : isas) {
				if (!(isa instanceof DAGNode))
					continue;
				int id = ((DAGNode) isa).getID();
				if (id == -1)
					continue;

				Integer count = isaArgCounts_[argIndex - 1].get(isa);
				if (count == null)
					count = 0;
				isaArgCounts_[argIndex - 1].put(isa, count + 1);
			}

			if (this != globalCounts_)
				globalCounts_.recordCount(edgeNode, argIndex, isas);
		}

		@Override
		public String toString() {
			return Arrays.toString(isaArgCounts_);
		}
	}
}
