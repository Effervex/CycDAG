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
import graph.core.cli.comparator.DepthComparator;
import graph.inference.CommonQuery;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.LoggerFactory;

public class PredicateRefinerModule extends DAGModule<Collection<DAGNode>> {
	private static final StringNode PREDICATE_REFINER_CREATOR = new StringNode(
			"PredicateRefinerModule");
	private static final long serialVersionUID = 1L;

	/** The query module access. */
	private transient QueryModule querier_;

	/** Related edge module access. */
	private transient RelatedEdgeModule relEdgeModule_;

	/** Global constraints. */
	private transient Collection<Node>[] globals_;

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
		// Identify Refinable Predicates
		Integer minEvidence = (Integer) args[0];
		Double threshold = (Double) args[1];

		// Calculate globals
		Collection<Node> refinables = CommonQuery.DIRECTINSTANCE.runQuery(dag_,
				CommonConcepts.REFINABLE_PREDICATE.getNode(dag_));
		if (refinables.isEmpty())
			return CollectionUtils.EMPTY_COLLECTION;

		if (globals_ == null)
			globals_ = calculateGlobal(refinables, threshold);

		Collection<DAGNode> refined = new ArrayList<>();
		if (args.length >= 3) {
			DAGNode singlePred = dag_.findDAGNode(args[2].toString());
			if (countAndInfer(singlePred, threshold, minEvidence))
				refined.add(singlePred);
			return refined;
		}

		// For each refinable predicate, process if sufficient evidence
		for (Node refPred : refinables) {
			if (countAndInfer((DAGNode) refPred, threshold, minEvidence))
				refined.add((DAGNode) refPred);
		}

		return refined;
	}

	/**
	 * Calculate global maximum collections by sampling a single edge from every
	 * refinable predicate, then inferring the constraints.
	 *
	 * @param refinables
	 *            The refinables to sample from.
	 * @param threshold
	 * @return A collection of at least one node representing the most general
	 *         constraints.
	 */
	private Collection<Node>[] calculateGlobal(Collection<Node> refinables,
			double threshold) {
		@SuppressWarnings("unchecked")
		Collection<Node>[] globals = new Collection[2];
		Collection<Edge> samples = new ArrayList<>();
		for (Node refPred : refinables) {
			Collection<Edge> edges = relEdgeModule_.findEdgeByNodes(refPred);
			if (edges.isEmpty())
				continue;
			Edge first = edges.iterator().next();
			samples.add(first);
		}
		// Inferring constraints for global edges
		NodeDetails globalDetails = recordEvidence(samples);
		for (int i = 0; i < 2; i++)
			globals[i] = globalDetails
					.inferConstraints(threshold, i + 1, false);
		LoggerFactory.getLogger(this.getClass()).info("Globals identified");
		return globals;
	}

	/**
	 * A method for counting and subsequently either refining a predicate,
	 * removing a useless predicate, or ignoring a predicate. This method
	 * differs from an earlier version in that it does not load every predicate
	 * in at once.
	 *
	 * @param refPred
	 *            The predicate to refine.
	 * @param threshold
	 *            The threshold to refine at.
	 * @param minEvidence
	 *            The minimum amount of evidence required to refine.
	 * @return True if the predicate created at least one refined arg
	 *         constraint.
	 */
	private boolean countAndInfer(DAGNode refPred, Double threshold,
			Integer minEvidence) {
		Collection<Edge> refEdges = relEdgeModule_.execute(refPred, 1);
		// If this is empty - just remove the predicate - it is useless
		if (refEdges.isEmpty()) {
			dag_.removeNode(refPred);
			return false;
		}
		// Not enough evidence
		if (refEdges.size() < minEvidence)
			return false;

		// Count the parents
		NodeDetails counts = recordEvidence(refEdges);
		boolean refined = false;
		// For each argument
		int bothRefined = 0;
		for (int i = 1; i <= 2; i++) {
			if (counts.numEvidence_[i - 1] < minEvidence)
				continue;
			Collection<Node> constraints = counts.inferConstraints(threshold,
					i, true);
			// Only create constraints not in the global set.
			for (Node constraint : constraints) {
				if (!isGeneralConstraint(constraint, globals_[i - 1])) {
					Edge e = ((CycDAG) dag_).findOrCreateEdge(new Node[] {
							CommonConcepts.ARGISA.getNode(dag_), refPred,
							PrimitiveNode.parseNode(i + ""), constraint },
							PREDICATE_REFINER_CREATOR, true, false);
					if (!(e instanceof ErrorEdge))
						refined = true;
				}
			}
			if (refined)
				bothRefined++;
		}

		// Impose the constraints
		if (refined) {
			int numEdges = imposeConstraints(refPred);
			LoggerFactory.getLogger(this.getClass()).info(
					"Refined {} ({} edges)", refPred.getName(), numEdges);
			System.out.println("Refined " + refPred.getName() + " (" + numEdges
					+ " edges)");
		}

		// Fully refined refinable predicate
		if (bothRefined == 2) {
			// Do nothing for now
		}

		return refined;
	}

	/**
	 * Checks if a constraint is contained within the set of global exclusive
	 * maximum constraints.
	 *
	 * @param constraint
	 *            The constraint to check.
	 * @param globalConstraints
	 *            The exclusive global constraints to compare against.
	 * @return True if the constraint is a global constraints.
	 */
	private boolean isGeneralConstraint(Node constraint,
			Collection<Node> globalConstraints) {
		if (globalConstraints.contains(constraint))
			return true;
		return false;
	}

	/**
	 * Imposes the constraints found for each refined predicate such that any
	 * edges not satisfying the constraints either have their arguments coerced
	 * into the correct collections, or removed.
	 *
	 * @param refined
	 *            The refined predicates to impose constraints for.
	 * @return The number of edges remaining after imposing constraints.
	 */
	public int imposeConstraints(DAGNode refPred) {
		Collection<Edge> refEdges = relEdgeModule_.execute(refPred, 1);
		int count = 0;
		for (Edge e : refEdges) {
			// Attempt to add the edge coercing arguments if necessary
			Edge testEdge = dag_.findOrCreateEdge(e.getNodes(), null, true,
					false, true);
			if (testEdge instanceof ErrorEdge) {
				// If the coercion failed, remove the edge
				dag_.removeEdge(e);
			} else
				count++;
		}
		return count;
	}

	/**
	 * Records the evidence for a RefinableNode, counting the isa evidence to be
	 * used later for inferring constraints.
	 * 
	 * @param refEdges
	 *            The edges corresponding to the refinable predicate
	 */
	public NodeDetails recordEvidence(Collection<Edge> refEdges) {
		NodeDetails refCounts = new NodeDetails();

		// Record each edge's evidence
		for (Edge edge : refEdges) {
			// For every argument
			Node[] args = ((DAGEdge) edge).getNodes();
			for (int i = 1; i < args.length; i++) {
				refCounts.recordCount(args[i], i, null);
			}
		}
		return refCounts;
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
		 * @param reduceToMin
		 *            If the constraints should be reduced to the minimal set.
		 *            Typically true, except when calculating globals.
		 * @return The set of all constraints for the given index.
		 */
		@SuppressWarnings("unchecked")
		public Collection<Node> inferConstraints(double threshold,
				int argIndex, boolean reduceToMin) {
			int minCount = (int) Math.ceil(threshold
					* numEvidence_[argIndex - 1]);
			ArrayList<Node> possible = new ArrayList<>();
			if (isaArgCounts_[argIndex - 1] == null)
				return possible;
			for (Object count : isaArgCounts_[argIndex - 1].entrySet()) {
				Map.Entry<Node, Integer> entry = (Entry<Node, Integer>) count;
				if (entry.getValue() >= minCount)
					possible.add(entry.getKey());
			}
			if (reduceToMin) {
				// Sort by depth
				Collections.sort(possible, new Comparator<Node>() {
					@Override
					public int compare(Node o1, Node o2) {
						DAGNode do1 = (DAGNode) o1;
						DAGNode do2 = (DAGNode) o2;
						int depth1 = extractDepth(do1);
						int depth2 = extractDepth(do2);
						int result = Integer.compare(depth1, depth2);
						if (result != 0)
							return -result;

						return do1.compareTo(do2);
					}

					private int extractDepth(DAGNode n) {
						int depth1 = Integer.MAX_VALUE;
						String depthStr1 = n
								.getProperty(DepthModule.DEPTH_PROPERTY);
						if (depthStr1 == null)
							return depth1;
						try {
							depth1 = Integer.parseInt(depthStr1);
						} catch (Exception e) {
						}
						return depth1;
					}
				});
				return (Collection<Node>) CommonQuery.minGeneralFilter(
						possible, dag_);
			} else
				return possible;
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
				// If a global, ignore it
				if (globals_ != null && globals_[argIndex - 1].contains(isa))
					continue;
				int id = ((DAGNode) isa).getID();
				if (id == -1)
					continue;

				Integer count = isaArgCounts_[argIndex - 1].get(isa);
				if (count == null)
					count = 0;
				isaArgCounts_[argIndex - 1].put(isa, count + 1);
			}
		}

		@Override
		public String toString() {
			return Arrays.toString(isaArgCounts_);
		}
	}
}
