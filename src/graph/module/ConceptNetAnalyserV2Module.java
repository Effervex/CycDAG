package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.StringNode;
import graph.inference.CommonQuery;
import graph.inference.QueryResult;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import util.Pair;
import util.collection.MultiMap;

/**
 * A special class which isn't really interactive, save for the one execute
 * command for it to start. This is a reimplementation of Yichen's code (his
 * code is rather messy, so giving it a cleaner and more efficient rewrite).
 * This should let me understand the code too.
 *
 * @author Sam Sarjant
 */
public class ConceptNetAnalyserV2Module extends DAGModule<Boolean> {
	private static final Node CREATOR = new StringNode("DisjointDiscovery");
	private static final File DISJOINT_OUT = new File("createdDisjoints.txt");
	private static final Pattern RELATION_PATTERN = Pattern
			.compile("/a/\\[.+?\\]\t/r/(\\S+)\t/c/[a-z]+/([^ \t/]+)\\S*\t/c/[a-z]+/([^ \t/]+)\t\\S+\t[\\d.]+.+?$");
	private static final long serialVersionUID = 1L;

	public static final int CONJOINT = 1;
	public static final int DISJOINT = 0;
	public static final int UNKNOWN = 2;

	private final double _RELIABILITYTHRESHOLD = 0.95;
	private transient int _resolvedCount = 0;
	private final int _STATMIN = 30;

	private transient NodeAliasModule aliasModule_;

	/** The pairs already processed. */
	private transient MultiMap<String, Pair<DAGNode, DAGNode>> explored_;
	/** The unresolved nodes that have isa parents */
	private MultiMap<String, DAGNode> isaEdges_;
	/** The children of partially tangible. */
	private transient Collection<Node> ptChildren_;

	/** The QueryModule access. */
	private transient QueryModule queryModule_;

	/** The reliability of relations. */
	private transient Map<String, RelationData> relationCounts_;

	/** The strings in ConceptNet5 that resolve to one or more nodes. */
	private transient MultiMap<String, DAGNode> resolvedConcepts_;

	/**
	 * Calculates the common parents of two nodes.
	 *
	 * @param nodeA
	 *            A child.
	 * @param nodeB
	 *            A child.
	 * @return The common parent of the two children.
	 */
	private Collection<DAGNode> calculateCommon(DAGNode nodeA, DAGNode nodeB) {
		Collection<DAGNode> commonParents = new ArrayList<>();
		for (Node n : CommonQuery.COMMONGENLS.runQuery(dag_, nodeA, nodeB))
			commonParents.add((DAGNode) n);
		commonParents.retainAll(ptChildren_);
		return commonParents;
	}

	/**
	 * Creates a disjoint edge from two nodes.
	 *
	 * @param objA
	 *            The left side of the disjoint edge.
	 * @param objB
	 *            The right side of the disjoint edge.
	 * @param assertDisj
	 * @throws IOException
	 *             Should something go awry...
	 */
	private void createDisjointEdge(DAGNode objA, DAGNode objB,
			BufferedWriter out, boolean assertDisj) throws IOException {
		Node[] edgeNodes = { CommonConcepts.DISJOINTWITH.getNode(dag_), objA,
				objB };
		out.write("(" + StringUtils.join(edgeNodes, ' ') + ")\n");
		if (assertDisj)
			dag_.findOrCreateEdge(edgeNodes, CREATOR, true, false);
	}

	/**
	 * Disambiguates the concept into one or more nodes.
	 *
	 * @param conceptName
	 *            The name of the concept.
	 * @param useIsas
	 *            If the isa data should be used to get the concept if it cannot
	 *            be found.
	 * @return The disambiguations for the concept or an empty collection.
	 */
	@SuppressWarnings("unchecked")
	private Collection<DAGNode> disambiguate(String conceptName, boolean useIsas) {
		if (resolvedConcepts_ == null)
			resolvedConcepts_ = MultiMap.createConcurrentHashSetMultiMap();
		if (resolvedConcepts_.containsKey(conceptName))
			return resolvedConcepts_.get(conceptName);

		// Disambiguate
		Collection<DAGNode> nodes = aliasModule_.findNodes(conceptName, false,
				true);
		if (!nodes.isEmpty()) {
			// Return all disambiguations
			resolvedConcepts_.putCollection(conceptName, nodes);
			return nodes;
		} else if (useIsas) {
			// Return isa edges if applicable.
			nodes = isaEdges_.get(conceptName);
			if (nodes != null)
				return nodes;
		}

		return CollectionUtils.EMPTY_COLLECTION;
	}

	/**
	 * Initialises the relation data structure.
	 *
	 * @param relation
	 *            The relation to initialise
	 */
	private synchronized void initRelationData(String relation) {
		if (!relationCounts_.containsKey(relation)) {
			relationCounts_.put(relation, new RelationData(relation));
		}
	}

	private boolean isReliable(int[] counts, float threshold) {
		return counts[DISJOINT] / (1f + counts[DISJOINT] + counts[CONJOINT]) >= threshold;
	}

	private boolean isSignificant(int[] counts, int threshold) {
		return counts[DISJOINT] + counts[CONJOINT] >= threshold;
	}

	protected boolean shouldCreateDisjoint(Pair<DAGNode, DAGNode> unknown,
			RelationData rData) {
		Collection<DAGNode> unknownParents = rData.unknownParents_.get(unknown);
		// Remove non-significant parents
		for (Iterator<DAGNode> iter = unknownParents.iterator(); iter.hasNext();) {
			DAGNode parent = iter.next();
			if (!isSignificant(rData.intraNodeCounts_.get(parent), _STATMIN))
				iter.remove();
		}
		// Filter to min collection
		Collection<? extends Node> minParents = CommonQuery.minGeneralFilter(
				unknownParents, dag_);
		// Check reliability of each parent
		for (Node minPar : minParents) {
			if (!isReliable(rData.intraNodeCounts_.get(minPar), 0.9f))
				return false;
		}
		return true;
	}

	/**
	 * Infer disjointness from the relations through counts of known disjoint
	 * and conjoint pairs.
	 *
	 * @param dataFolder
	 *            The location of the data files.
	 */
	public void countDisjointness(File dataFolder) {
		System.out.println("Disjointness Discovery: Calculating "
				+ "disjoint/conjoint/unknown pairs from ConceptNet5.");
		ExecutorService executor = Executors.newFixedThreadPool(Runtime
				.getRuntime().availableProcessors());
		File[] cnFiles = dataFolder.listFiles();
		for (File file : cnFiles) {
			Runnable worker = new ProcessRelationFile(file);
			executor.execute(worker);

		}
		executor.shutdown();
		while (!executor.isTerminated()) {
			try {
				Thread.sleep(10000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		System.out.println("Disjointness Discovery: Calculation complete!");
	}

	/**
	 * Creates the disjointness from the inferred counts.
	 * 
	 * @param assertDisj
	 *            If the assertions should occur immediately.
	 */
	public void createDisjointness(boolean assertDisj) {
		System.out.println("Disjointness Discovery: Creating disjoint edges "
				+ "from ConceptNet5.");
		int numCreated = 0;
		try {
			DISJOINT_OUT.createNewFile();
			BufferedWriter out = new BufferedWriter(
					new FileWriter(DISJOINT_OUT));
			for (RelationData rData : relationCounts_.values()) {
				if (isSignificant(rData.counts_, _STATMIN)
						&& isReliable(rData.counts_, .95f)) {
					// Run through the unknown pairs
					for (Pair<DAGNode, DAGNode> unknown : rData.unknownParents_
							.keySet()) {
						if (shouldCreateDisjoint(unknown, rData)) {
							numCreated++;
							createDisjointEdge(unknown.objA_, unknown.objB_,
									out, assertDisj);
						}
					}
				}
			}
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println("Disjointness Discovery: " + numCreated
				+ " disjoint edges created!");
	}

	@Override
	public Boolean execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		queryModule_ = (QueryModule) dag_.getModule(QueryModule.class);
		aliasModule_ = (NodeAliasModule) dag_.getModule(NodeAliasModule.class);
		explored_ = MultiMap.createConcurrentHashSetMultiMap();
		relationCounts_ = new HashMap<>();
		ptChildren_ = CommonQuery.SPECS.runQuery(dag_,
				CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));

		if (args.length == 0)
			return false;
		File dataFolder = new File(args[0].toString());
		boolean assertDisj = (boolean) args[1];
		readIsaData(dataFolder);
		countDisjointness(dataFolder);
		createDisjointness(assertDisj);

		return true;
	}

	/**
	 * Reads the native isa data from the relations.
	 *
	 * @param dataFolder
	 *            The location of the data files.
	 */
	public void readIsaData(File dataFolder) {
		// Already know isas.
		if (isaEdges_ != null)
			return;

		// Read in and store IsA relations
		System.out.println("Disjointness Discovery: Reading in IsA data "
				+ "from ConceptNet5");
		File[] cnFiles = dataFolder.listFiles();
		for (File file : cnFiles) {
			try {
				BufferedReader in = new BufferedReader(new FileReader(file));
				String line = null;

				while ((line = in.readLine()) != null) {
					// Find IsA relations only
					Matcher m = RELATION_PATTERN.matcher(line);
					if (m.matches()) {
						String relation = m.group(1);
						if (!relation.equals("IsA"))
							continue;

						String first = m.group(2);
						String second = m.group(3);

						Collection<DAGNode> firstNodes = disambiguate(first,
								false);
						Collection<DAGNode> secondNodes = disambiguate(second,
								false);

						// Proceed if second arg known
						if (!secondNodes.isEmpty()) {
							if (firstNodes.isEmpty()) {
								// If first is ambiguous, note the Isa Edge
								if (isaEdges_ == null)
									isaEdges_ = MultiMap
											.createSortedSetMultiMap();
								isaEdges_.putCollection(first, secondNodes);
							} else {
								_resolvedCount++;
							}
						}
					}
				}
				in.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		System.out.println("Disjointness Discovery: "
				+ "Reading IsA data complete!");
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return false;
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}

	private class ProcessRelationFile implements Runnable {
		private File file_;
		private int finalPairsCount_ = 0;
		private int notFoundCount_ = 0;
		private int resolvedCount_ = 0;

		public ProcessRelationFile(File file) {
			file_ = file;
		}

		/**
		 * Converts the nodes into collections via MINISA, if applicable.
		 *
		 * @param nodes
		 *            The nodes to convert to collections.
		 * @return The nodes themselves, if they are collections, or MINISA
		 *         parents.
		 */
		private Collection<DAGNode> asCollections(Collection<DAGNode> nodes) {
			// For every node, convert to collection
			Collection<DAGNode> collections = new HashSet<>();
			for (DAGNode node : nodes) {
				if (queryModule_.prove(false, CommonConcepts.ISA.getNode(dag_),
						node, CommonConcepts.COLLECTION.getNode(dag_)) == QueryResult.TRUE) {
					collections.add(node);
				} else {
					for (Node isa : CommonQuery.MINISA.runQuery(dag_, node))
						collections.add((DAGNode) isa);
				}
			}

			// Remove shallow nodes and materials
			DAGNode pt = CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_);
			int ptDepth = Integer.parseInt(pt
					.getProperty(DepthModule.DEPTH_PROPERTY));
			DAGNode material = dag_
					.findDAGNode("CommonSubstances-Material-Topic");
			for (Iterator<DAGNode> iter = collections.iterator(); iter
					.hasNext();) {
				DAGNode node = iter.next();
				// Remove shallow nodes and non-descendants of
				// PartiallyTangible.
				String depthStr = node.getProperty(DepthModule.DEPTH_PROPERTY);
				if (depthStr == null
						|| Integer.parseInt(depthStr) <= ptDepth
						|| queryModule_.prove(false,
								CommonConcepts.GENLS.getNode(dag_), node, pt) != QueryResult.TRUE) {
					iter.remove();
					continue;
				}
				// Remove instances of material
				if (queryModule_.prove(false, CommonConcepts.ISA.getNode(dag_),
						node, material) == QueryResult.TRUE) {
					iter.remove();
					continue;
				}
			}

			return collections;
		}

		/**
		 * Processes a potentially disjoint pair for a given relation.
		 *
		 * @param relation
		 *            The relation for the pair.
		 * @param pair
		 *            The ordered args of the relation.
		 */
		private void processPair(String relation, Pair<DAGNode, DAGNode> pair) {
			synchronized (this) {
				if (explored_.containsKey(relation)
						&& explored_.get(relation).contains(pair))
					return;
				// Record explored.
				explored_.put(relation, pair);
			}

			QueryResult qr = queryModule_.prove(true,
					CommonConcepts.DISJOINTWITH.getNode(dag_), pair.objA_,
					pair.objB_);
			int flag = UNKNOWN;
			if (qr == QueryResult.TRUE)
				flag = DISJOINT;
			else if (qr == QueryResult.FALSE)
				flag = CONJOINT;
			else
				flag = UNKNOWN;

			// Add count to relation
			RelationData relData = relationCounts_.get(relation);
			relData.incrementRelation(flag);

			// Calculate common parents
			Collection<DAGNode> common = calculateCommon(pair.objA_, pair.objB_);
			for (DAGNode parent : common)
				relData.addIntraParent(parent, flag);

			// Note unknown pairs for later disjoint edge creation
			if (flag == UNKNOWN)
				relData.addUnknownData(pair, common);
		}

		protected void processLine(String line) {
			// Find IsA relations only
			Matcher m = RELATION_PATTERN.matcher(line);
			if (m.matches()) {
				String relation = m.group(1);
				if (relation.equals("IsA"))
					return;

				String first = m.group(2);
				String second = m.group(3);

				// Disambiguate nodes
				Collection<DAGNode> firstNodes = disambiguate(first, true);
				if (firstNodes.isEmpty()) {
					notFoundCount_++;
					return;
				}
				Collection<DAGNode> secondNodes = disambiguate(second, true);
				if (secondNodes.isEmpty()) {
					notFoundCount_++;
					return;
				}
				resolvedCount_++;

				// Find left and right disjoint candidates
				Collection<DAGNode> firstDisjoint = asCollections(firstNodes);
				Collection<DAGNode> secondDisjoint = asCollections(secondNodes);

				// For every disjoint pair
				initRelationData(relation);
				for (DAGNode firstNode : firstDisjoint) {
					for (DAGNode secondNode : secondDisjoint) {
						Pair<DAGNode, DAGNode> pair = (firstNode.hashCode() < secondNode
								.hashCode()) ? new Pair<DAGNode, DAGNode>(
								firstNode, secondNode)
								: new Pair<DAGNode, DAGNode>(secondNode,
										firstNode);
						finalPairsCount_++;
						processPair(relation, pair);
					}
				}
			}
		}

		@Override
		public void run() {
			// Read each relation from the file and infer disjointness
			try {
				BufferedReader in = new BufferedReader(new FileReader(file_));
				String line = null;

				while ((line = in.readLine()) != null) {
					processLine(line);
				}
				System.out.println("Completed processing file '" + file_ + "'");
				in.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private class RelationData {

		public int[] counts_ = new int[3];
		public Map<DAGNode, int[]> intraNodeCounts_;
		public String relation_;
		public MultiMap<Pair<DAGNode, DAGNode>, DAGNode> unknownParents_;

		public RelationData(String relation) {
			relation_ = relation;
			intraNodeCounts_ = new ConcurrentHashMap<>();
			unknownParents_ = MultiMap.createConcurrentHashSetMultiMap();
		}

		public void addIntraParent(DAGNode parent, int flag) {
			int[] counts = intraNodeCounts_.get(parent);
			if (!intraNodeCounts_.containsKey(parent)) {
				counts = new int[3];
				intraNodeCounts_.put(parent, counts);
			}
			counts[flag]++;
		}

		public void addUnknownData(Pair<DAGNode, DAGNode> pair,
				Collection<DAGNode> common) {
			unknownParents_.putCollection(pair, common);
		}

		public void incrementRelation(int flag) {
			counts_[flag]++;
		}

		@Override
		public String toString() {
			DecimalFormat df = new DecimalFormat("#.##");
			float reliability = counts_[DISJOINT]
					/ (1f + counts_[DISJOINT] + counts_[CONJOINT]);
			return relation_ + " - Counts: " + Arrays.toString(counts_) + " "
					+ df.format(reliability) + ", # intra: "
					+ intraNodeCounts_.size();
		}
	}
}
