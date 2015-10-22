package graph.module;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.inference.QueryObject;
import graph.inference.QueryResult;

/**
 * A module for producing groups of consistent collections, similar to
 * KnowledgeMiner's Disjointness Disambiguation procedure. Given an input of
 * either plain text (which is disambiguated), ontological Name/ID, or other
 * linked resources, the class locates and returns all consistent sets of the
 * group, according to disjointness.
 * 
 * Note that this is an NP-complete problem, so do not overload it.
 *
 * @author Sam Sarjant
 */
public class DisjointDisambiguationModule extends
		DAGModule<Collection<Collection<DAGNode>>> {
	private static final long serialVersionUID = 1L;
	private transient NodeAliasModule nodeAliasModule_;
	private transient QueryModule queryModule_;

	/**
	 * The method for clustering the groups together. TODO Note that the
	 * clustering is not exhaustive - it is possible that other subsets exist.
	 * The clustering only guarantees that every disambiguation is included in
	 * at least one set.
	 *
	 * @param candidates
	 *            The candidates to cluster.
	 * @return A collection of groups of ontological concepts.
	 */
	@SuppressWarnings("rawtypes")
	private Collection<Collection<DAGNode>> clusterCollections(
			DAGNode[][] candidates) {
		SortedSet<Collection<DAGNode>> groups = new TreeSet<>(
				new Comparator<Collection>() {
					@Override
					public int compare(Collection o1, Collection o2) {
						int result = Integer.compare(o1.size(), o2.size());
						if (result != 0)
							return -result;
						return o1.toString().compareTo(o2.toString());
					}
				});

		// Find consistent groups
		Map<String, QueryResult> cache = new HashMap<>();
		Set<DAGNode> grouped = new HashSet<>();
		for (int i = 0; i < candidates.length; i++) {
			if (candidates[i] == null)
				continue;
			for (int y = 0; y < candidates[i].length; y++) {
				if (grouped.contains(candidates[i][y]))
					continue;

				// Starting with this fact, check disjointness to all others
				Collection<DAGNode> group = groupConsistent(candidates[i][y],
						candidates, i, cache);
				grouped.addAll(group);
				groups.add(group);
				continue;
			}
		}

		return groups;
	}

	/**
	 * Disambiguates all arguments into ontological collections
	 *
	 * @param args
	 *            The arguments to disambiguate.
	 * @param caseSensitive
	 * @return An array of arguments, 2 dimensional for when an argument
	 *         disambiguates to multiple arguments. Non-disambiguatable terms
	 *         are null.
	 */
	private DAGNode[][] disambiguateArguments(Object[] args,
			boolean caseSensitive) {
		DAGNode[][] disams = new DAGNode[args.length][];
		for (int i = 0; i < args.length; i++) {
			if (args[i] instanceof DAGNode && isCollection((DAGNode) args[i]))
				disams[i] = new DAGNode[] { (DAGNode) args[i] };
			else {
				try {
					DAGNode resourceBound = disambiguateSpecialString(args[i]
							.toString());
					if (isCollection(resourceBound))
						disams[i] = new DAGNode[] { resourceBound };
				} catch (UnknownMarkupException ume) {
					// Disambiguate text
					Collection<DAGNode> collections = new ArrayList<>();
					Collection<DAGNode> results = nodeAliasModule_.findNodeByAlias(
							args[i].toString(), caseSensitive, true, true);
					for (DAGNode alias : results) {
						if (isCollection(alias))
							collections.add(alias);
					}

					// Record each
					if (!collections.isEmpty())
						disams[i] = collections.toArray(new DAGNode[collections
								.size()]);
				}
			}
		}
		return disams;
	}

	/**
	 * Passes through the candidates, checking consistency against each other
	 * candidate. All consistent candidates are added to the group.
	 *
	 * @param seedNode
	 *            The seed node to check against.
	 * @param candidates
	 *            The candidates to group.
	 * @param i
	 *            The candidate index to ignore
	 * @param cache
	 *            Disjoint query cache.
	 * @return A consistent group of size 1 or more.
	 */
	private Collection<DAGNode> groupConsistent(DAGNode seedNode,
			DAGNode[][] candidates, int i, Map<String, QueryResult> cache) {
		Collection<DAGNode> group = new HashSet<>();
		group.add(seedNode);
		for (int x = 0; x < candidates.length; x++) {
			if (candidates[x] == null || x == i)
				continue;
			for (int y = 0; y < candidates[x].length; y++) {
				if (isConsistent(candidates[x][y], group, cache)) {
					group.add(candidates[x][y]);
					break;
				}
			}
		}
		return group;
	}

	private boolean isCollection(DAGNode node) {
		QueryObject qo = new QueryObject(false, false, QueryResult.TRUE,
				CommonConcepts.ISA.getNode(dag_), node,
				CommonConcepts.COLLECTION.getNode(dag_));
		return queryModule_.prove(qo) == QueryResult.TRUE;
	}

	/**
	 * Checks consistency of the current node against all other collections in
	 * the group.
	 *
	 * @param comparisonNode
	 *            The node to check consistency against
	 * @param group
	 *            The group against whcih to check.
	 * @param cache
	 *            Disjoint query cache
	 * @return True if the node is consistent with all nodes in the group. False
	 *         otherwise.
	 */
	private boolean isConsistent(DAGNode comparisonNode,
			Collection<DAGNode> group, Map<String, QueryResult> cache) {
		String compID = comparisonNode.getIdentifier();
		for (DAGNode n : group) {
			if (n.equals(comparisonNode))
				continue;

			// Create cache string to check for existing results.
			String nID = n.getIdentifier();
			String cacheStr = (nID.compareTo(compID) < 0) ? nID + compID
					: compID + nID;
			QueryResult result = null;
			if (!cache.containsKey(cacheStr)) {
				// Ask the disjoint query
				QueryObject qo = new QueryObject(false, false,
						QueryResult.TRUE,
						CommonConcepts.DISJOINTWITH.getNode(dag_),
						comparisonNode, n);
				result = queryModule_.prove(qo);
				cache.put(cacheStr, result);
			} else
				result = cache.get(cacheStr);
			if (result == QueryResult.TRUE)
				return false;
		}
		return true;
	}

	/**
	 * Disambiguates a collection of arguments into consistent groups. Note that
	 * no group should be a subset of another group.
	 *
	 * @param args
	 *            The arguments - in plain text (either disambiguated or in
	 *            special encoded format for external resource), ontological
	 *            concept
	 * @return Groups of consistent assertions.
	 */
	public Collection<Collection<DAGNode>> disambiguateGroups(Object[] args,
			boolean caseSensitive) {
		// Disambiguate each argument to a collection of ontological Collections
		DAGNode[][] candidates = disambiguateArguments(args, caseSensitive);
		// Create the groups
		return clusterCollections(candidates);
	}

	/**
	 * Disambiguates a special encoded string into the appropriate DAGNode
	 * resource (if one exists). If the string cannot be decoded, throws an
	 * Exception.
	 *
	 * @param codedString
	 *            The coded string to be disambiguated into a DAGNode
	 * @return The disambiguated DAGNode, null if no disambiguation exists, or
	 *         throws a UnknownMarkupException
	 */
	public DAGNode disambiguateSpecialString(String codedString)
			throws UnknownMarkupException {
		if (codedString.startsWith(DDResource.WIKIPEDIA.prefix_)) {
			// TODO Finish this at some point. Kinda tied to KM, so decouple it
		}
		throw new UnknownMarkupException();
	}

	@Override
	public Collection<Collection<DAGNode>> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		return disambiguateGroups(Arrays.copyOf(args, args.length - 1),
				(boolean) args[args.length - 1]);
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);
		nodeAliasModule_ = (NodeAliasModule) directedAcyclicGraph
				.getModule(NodeAliasModule.class);
		queryModule_ = (QueryModule) directedAcyclicGraph
				.getModule(QueryModule.class);
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return false;
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}

	/**
	 * A method for encoding resources into a parsable format.
	 *
	 * @param location
	 *            The resource location.
	 * @param type
	 *            The type of encoding.
	 * @return A string representing the encoded text that can later be decoded.
	 */
	public static String encodeSpecialString(String location, DDResource type) {
		return type.prefix_ + ":" + location;
	}

	/**
	 * An enum for special resources that can be mapped to ontological concepts.
	 */
	public enum DDResource {
		WIKIPEDIA("wiki");

		private String prefix_;

		private DDResource(String prefix) {
			prefix_ = prefix;
		}
	}
}
