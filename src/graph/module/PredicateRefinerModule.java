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
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
import graph.core.StringNode;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.QueryResult;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.LoggerFactory;

public class PredicateRefinerModule extends DAGModule<Collection<Edge>> {
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
	@Override
	public Collection<Edge> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		// Identify Refinable Predicates
		Integer minEvidence = (Integer) args[0];
		Float threshold = (Float) args[1];
		boolean assertConstraints = (Boolean) args[2];
		String sourceKey = (String) args[3];
		String predicate = null;
		if (args.length == 5)
			predicate = args[4].toString();

		return refinePredicates(minEvidence, threshold, assertConstraints,
				sourceKey, predicate);
	}

	@SuppressWarnings("unchecked")
	public Collection<Edge> refinePredicates(int minEvidence, float threshold,
			boolean assertConstraints, String sourceKey, String predicate) {
		// Calculate globals
		Collection<Node> refinables = CommonQuery.DIRECTINSTANCE.runQuery(dag_,
				CommonConcepts.REFINABLE_PREDICATE.getNode(dag_));
		if (refinables.isEmpty())
			return CollectionUtils.EMPTY_COLLECTION;

		if (globals_ == null)
			globals_ = calculateGlobal(refinables, threshold);

		if (predicate != null) {
			DAGNode predNode = dag_.findDAGNode(predicate);
			return countAndInfer(predNode, threshold, minEvidence,
					assertConstraints, sourceKey);
		}

		// For each refinable predicate, process if sufficient evidence
		Collection<Edge> constraints = new ArrayList<>();
		for (Node refPred : refinables) {
			constraints.addAll(countAndInfer((DAGNode) refPred, threshold,
					minEvidence, assertConstraints, sourceKey));
		}

		return constraints;

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
		NodeDetails globalDetails = recordEvidence(samples, null);
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
	private Collection<Edge> countAndInfer(DAGNode refPred, float threshold,
			int minEvidence, boolean assertConstraints, String sourceKey) {
		Collection<Edge> constraints = new ArrayList<>();
		Collection<Edge> refEdges = relEdgeModule_.execute(refPred, 1);
		// If this is empty - just remove the predicate - it is useless
		if (refEdges.isEmpty()) {
			if (assertConstraints)
				dag_.removeNode(refPred);
			return constraints;
		}
		// Not enough evidence
		if (refEdges.size() < minEvidence)
			return constraints;

		// Count the parents
		NodeDetails counts = recordEvidence(refEdges, sourceKey);
		boolean refined = false;
		// For each argument
		int bothRefined = 0;
		for (int i = 1; i <= 2; i++) {
			if (counts.numEvidence_[i - 1] < minEvidence)
				continue;
			Collection<Node> nodeConstraints = counts.inferConstraints(
					threshold, i, true);
			// Only create constraints not in the global set.
			for (Node constraint : nodeConstraints) {
				Node[] nodes = new Node[] {
						CommonConcepts.ARGISA.getNode(dag_), refPred,
						PrimitiveNode.parseNode(i + ""), constraint };
				if (assertConstraints) {
					Edge e = ((CycDAG) dag_).findOrCreateEdge(nodes,
							PREDICATE_REFINER_CREATOR, true, false);
					if (!(e instanceof ErrorEdge)) {
						refined = true;
						constraints.add(e);
					}
				} else
					constraints.add(new OntologyFunction(nodes));
			}
			if (refined)
				bothRefined++;
		}

		// Impose the constraints
		if (refined && assertConstraints) {
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

		return constraints;
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
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public NodeDetails recordEvidence(Collection<Edge> refEdges,
			String sourceKey) {
		NodeDetails refCounts = new NodeDetails();

		// Sort the edges by source property
		Collection<Edge> sortedEdges = refEdges;
		if (sourceKey != null) {
			sortedEdges = new TreeSet<>(new SourceComparator(sourceKey));
			sortedEdges.addAll(refEdges);
		}

		// Record each edge's evidence
		String currentSource = null;
		Collection[] currentIsas = { new HashSet<Node>(), new HashSet<Node>() };
		Iterator<Edge> iter = sortedEdges.iterator();
		Edge edge = null;
		do {
			edge = (iter.hasNext()) ? iter.next() : null;
			// Check the edge source first
			String edgeSource = (edge != null) ? ((DAGEdge) edge)
					.getProperty(sourceKey) : null;
			if (edgeSource == null || !edgeSource.equals(currentSource)) {
				refCounts.incrementCounts(currentIsas);
				currentIsas[0].clear();
				currentIsas[1].clear();
			}
			currentSource = edgeSource;

			if (edge == null)
				break;

			// Note the isas for the edge
			Node[] args = ((DAGEdge) edge).getNodes();
			for (int i = 1; i < args.length; i++) {
				currentIsas[i - 1].addAll(CommonQuery.ALLISA.runQuery(dag_,
						querier_.getExpanded(args[i])));
			}
		} while (edge != null);
		return refCounts;
	}

	private class SourceComparator<T> implements Comparator<DAGEdge> {
		private String sourceKey_;

		public SourceComparator(String sourceKey) {
			sourceKey_ = sourceKey;
		}

		@Override
		public int compare(DAGEdge o1, DAGEdge o2) {
			if (o1 == null)
				if (o2 == null)
					return 0;
				else
					return 1;
			else if (o2 == null)
				return -1;

			// Compare by sourceKey
			String prop1 = o1.getProperty(sourceKey_);
			String prop2 = o2.getProperty(sourceKey_);
			if (prop1 == null) {
				if (prop2 != null)
					return 1;
			} else if (prop2 == null)
				return -1;
			int result = prop1.compareTo(prop2);
			if (result != 0)
				return result;

			return o1.getIdentifier().compareTo(o2.getIdentifier());
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

		/** Evidence counts for each argument of the node. */
		private int[] numEvidence_;

		@SuppressWarnings("unchecked")
		public NodeDetails() {
			isaArgCounts_ = new Map[] { new HashMap<Node, Integer>(),
					new HashMap<Node, Integer>() };
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
				Collection<Node> minConstraints = (Collection<Node>) CommonQuery
						.minGeneralFilter(possible, dag_);
				// Need to check disjointness among the constraints
				return checkDisjointConstraints(minConstraints);
			} else
				return possible;
		}

		/**
		 * Checks if the inferred constraints are disjoint to one-another. If
		 * so, <DO SOMETHING>
		 *
		 * @param minConstraints
		 *            The constraints to check for disjointness
		 * @return A set of constraints that are not disjoint.
		 */
		@SuppressWarnings("unchecked")
		private Collection<Node> checkDisjointConstraints(
				Collection<Node> minConstraints) {
			// One constraint is fine
			if (minConstraints.size() <= 1)
				return minConstraints;

			Node[] constraintArray = minConstraints
					.toArray(new Node[minConstraints.size()]);
			for (int i = 0; i < constraintArray.length - 1; i++) {
				for (int j = i + 1; j < constraintArray.length; j++) {
					QueryObject queryObj = new QueryObject(false, false,
							QueryResult.TRUE,
							CommonConcepts.DISJOINTWITH.getNode(dag_),
							constraintArray[i], constraintArray[j]);
					if (querier_.prove(queryObj) == QueryResult.TRUE) {
						// Exit with an empty set
						// TODO Could do something smarter here - somehow...
						return CollectionUtils.EMPTY_COLLECTION;
					}
				}
			}
			return minConstraints;
		}

		/**
		 * Records a count, creating new maps if necessary.
		 *
		 * @param isaNode
		 *            The node to record a count for.
		 * @param argIndex
		 *            The index to record under.
		 */
		public void incrementCounts(Collection<Node>[] isas) {
			for (int i = 0; i < isas.length; i++) {
				if (globals_ != null)
					isas[i].removeAll(globals_[i]);
				if (isas[i].isEmpty())
					return;

				numEvidence_[i]++;

				// Record counts
				for (Node isa : isas[i]) {
					if (!(isa instanceof DAGNode))
						continue;
					int id = ((DAGNode) isa).getID();
					if (id == -1)
						continue;

					Integer count = isaArgCounts_[i].get(isa);
					if (count == null)
						count = 0;
					isaArgCounts_[i].put(isa, count + 1);
				}
			}
		}

		@Override
		public String toString() {
			return Arrays.toString(isaArgCounts_);
		}
	}
}
