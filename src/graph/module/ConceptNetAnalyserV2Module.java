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
import java.io.LineNumberReader;
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
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import util.Pair;
import util.UtilityMethods;
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
	private static final String PARSED = "parsed";
	private static final String ARFF_OUT = "DisjointData.arff";

	private final int _STATMIN = 30;

	private transient NodeAliasModule aliasModule_;

	/** The pairs already processed. */
	private transient MultiMap<String, Pair<DAGNode, DAGNode>> explored_;
	/** The unresolved nodes that have isa parents */
	private transient MultiMap<String, DAGNode> isaEdges_;
	/** The children of partially tangible. */
	private transient Collection<Node> ptChildren_;

	/** The QueryModule access. */
	private transient QueryModule queryModule_;
	private boolean assertDisj_ = false;

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
	 *            If the edges should be asserted to the DAG.
	 * @return The assertion edge.
	 * @throws IOException
	 *             Should something go awry...
	 */
	private String createDisjointEdge(DAGNode objA, DAGNode objB,
			boolean assertDisj) throws IOException {
		Node[] edgeNodes = { CommonConcepts.DISJOINTWITH.getNode(dag_), objA,
				objB };
		String edge = "(" + StringUtils.join(edgeNodes, ' ') + ")";
		if (assertDisj)
			dag_.findOrCreateEdge(edgeNodes, CREATOR, true, false);
		return edge;
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
		// Disambiguate
		Collection<DAGNode> nodes = aliasModule_.findNodes(conceptName, false,
				true);

		if (!nodes.isEmpty()) {
			// Return all disambiguations
			return nodes;
		} else if (useIsas) {
			// TODO Return isa edges if applicable.
			// nodes = isaEdges_.get(conceptName);
			// if (nodes != null)
			// return nodes;
		}

		return CollectionUtils.EMPTY_COLLECTION;
	}

	private boolean isSignificant(int[] counts, int threshold) {
		return counts[DISJOINT] + counts[CONJOINT] >= threshold;
	}

	protected boolean shouldCreateDisjoint(UnknownPair unknownPair,
			RelationData rData, String[] outputString) {
		Collection<DAGNode> unknownParents = unknownPair.commonParents_;
		// Remove non-significant parents
		for (Iterator<DAGNode> iter = unknownParents.iterator(); iter.hasNext();) {
			DAGNode parent = iter.next();
			if (!isSignificant(
					rData.intraNodeCounts_.get(parent.getIdentifier(true)),
					_STATMIN))
				iter.remove();
		}

		// If common is empty, return false
		if (unknownParents.isEmpty())
			return false;

		// Filter to min collection
		Collection<? extends Node> minParents = CommonQuery.minGeneralFilter(
				unknownParents, dag_);
		// Check reliability of each parent
		outputString[RelationColumn.COMMON_PARENTS.ordinal()] = minParents
				.toString();
		float sumIntraConjCount = 0;
		float[] reliArray = new float[minParents.size()];
		float sum = 0;
		int i = 0;
		for (Node minPar : minParents) {
			int[] counts = rData.intraNodeCounts_.get(minPar
					.getIdentifier(true));
			reliArray[i] = counts[DISJOINT]
					/ (1f * counts[DISJOINT] + counts[CONJOINT]);
			sum += reliArray[i];
			sumIntraConjCount += counts[CONJOINT];
			i++;
		}
		outputString[RelationColumn.AV_PARENT_CONJ_COUNTS.ordinal()] = sumIntraConjCount
				/ minParents.size() + "";
		outputString[RelationColumn.PARENT_RELI.ordinal()] = Arrays
				.toString(reliArray);
		outputString[RelationColumn.AV_PARENT_RELI.ordinal()] = sum
				/ minParents.size() + "";
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
		File[] relationFiles = dataFolder.listFiles();
		for (File file : relationFiles) {
			if (file.getName().endsWith("IsA.txt"))
				continue;
			Runnable worker = new ProcessRelationFile(file);
			executor.execute(worker);
		}
		executor.shutdown();
		try {
			executor.awaitTermination(24, TimeUnit.HOURS);
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		System.out.println("Disjointness Discovery: Calculation complete!");
	}

	/**
	 * Creates the disjointness from the inferred counts.
	 * 
	 * @param assertDisj
	 *            If the assertions should occur immediately.
	 */
	public synchronized void createDisjointness(RelationData rData) {
		System.out
				.println("Disjointness Discovery: Creating disjoint edges for "
						+ rData.relation_);
		System.out.println(rData);
		int numCreated = 0;
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(
					DISJOINT_OUT, true));
			if (isSignificant(rData.counts_, _STATMIN)) {
				// Run through the unknown pairs
				for (UnknownPair unknown : rData.unknownPairs_) {
					String[] outputData = new String[RelationColumn.values().length];
					if (shouldCreateDisjoint(unknown, rData, outputData)) {
						numCreated++;
						outputData[RelationColumn.ARG1TEXT.ordinal()] = unknown.argA_;
						outputData[RelationColumn.ARG2TEXT.ordinal()] = unknown.argB_;
						if (rData.intraNodeCounts_.containsKey(unknown.nodeA_
								.getIdentifier(true)))
							outputData[RelationColumn.ARG1_CONJ_COUNT.ordinal()] = rData.intraNodeCounts_
									.get(unknown.nodeA_.getIdentifier(true))[CONJOINT]
									+ "";
						else
							outputData[RelationColumn.ARG1_CONJ_COUNT.ordinal()] = "0";
						if (rData.intraNodeCounts_.containsKey(unknown.nodeB_
								.getIdentifier(true)))
							outputData[RelationColumn.ARG2_CONJ_COUNT.ordinal()] = rData.intraNodeCounts_
									.get(unknown.nodeB_.getIdentifier(true))[CONJOINT]
									+ "";
						else
							outputData[RelationColumn.ARG2_CONJ_COUNT.ordinal()] = "0";

						outputData[RelationColumn.RELATION.ordinal()] = rData.relation_;
						outputData[RelationColumn.REL_DISJ.ordinal()] = rData.counts_[DISJOINT]
								+ "";
						outputData[RelationColumn.REL_CONJ.ordinal()] = rData.counts_[CONJOINT]
								+ "";
						outputData[RelationColumn.REL_UNKN.ordinal()] = rData.counts_[UNKNOWN]
								+ "";
						outputData[RelationColumn.REL_SIG.ordinal()] = (rData.counts_[DISJOINT] + rData.counts_[CONJOINT])
								+ "";
						outputData[RelationColumn.REL_RELIA.ordinal()] = (rData.counts_[DISJOINT] / (1f * rData.counts_[DISJOINT] + rData.counts_[CONJOINT]))
								+ "";
						outputData[RelationColumn.DISJOINT_EDGE.ordinal()] = createDisjointEdge(
								unknown.nodeA_, unknown.nodeB_, assertDisj_);
						out.append(StringUtils.join(outputData, '\t') + '\n');
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
		ptChildren_ = CommonQuery.SPECS.runQuery(dag_,
				CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));
		// Do not include PartiallyTangible itself.
		ptChildren_.remove(CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));

		if (args.length == 0)
			return false;
		File dataFolder = new File(args[0].toString() + File.separator + PARSED);
		if (!dataFolder.exists() || dataFolder.listFiles().length == 0)
			try {
				preParseConceptNetFiles(new File(args[0].toString()));
			} catch (IOException e) {
				e.printStackTrace();
			}
		assertDisj_ = (boolean) args[1];

		try {
			DISJOINT_OUT.createNewFile();
			BufferedWriter out = new BufferedWriter(
					new FileWriter(DISJOINT_OUT));
			out.write(StringUtils.join(RelationColumn.values(), '\t') + '\n');
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}

		// TODO readIsaData(dataFolder);
		countDisjointness(dataFolder);

		// Clean up
		explored_.clear();
		ptChildren_.clear();

		return true;
	}

	/**
	 * Reads the native isa data from the relations.
	 *
	 * @param parsedFolder
	 *            The location of the data files.
	 */
	public void readIsaData(File parsedFolder) {
		// Already know isas.
		if (isaEdges_ != null)
			return;

		// Read in and store IsA relations (from parsed)
		System.out.println("Disjointness Discovery: Reading in IsA data "
				+ "from ConceptNet5");
		File isaFile = new File(parsedFolder, "IsA.txt");
		try {
			BufferedReader in = new BufferedReader(new FileReader(isaFile));
			String line = null;

			while ((line = in.readLine()) != null) {
				// Find IsA relations only
				String[] split = line.split("\t");
				String first = split[1];
				String second = split[2];

				Collection<DAGNode> firstNodes = disambiguate(first, false);
				Collection<DAGNode> secondNodes = disambiguate(second, false);

				// Proceed if second arg known
				if (!secondNodes.isEmpty()) {
					if (firstNodes.isEmpty()) {
						// If first is ambiguous, note the Isa Edge
						if (isaEdges_ == null)
							isaEdges_ = MultiMap.createSortedSetMultiMap();
						isaEdges_.putCollection(first, secondNodes);
					}
				}
			}
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
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

	public static void preParseConceptNetFiles(File folder) throws IOException {
		if (!folder.exists()) {
			System.err.println(folder + " does not exist!");
			return;
		}
		Map<String, BufferedWriter> relationFiles = new HashMap<>();

		// Run through each file and output the relations in the appropriate
		// relation file
		File parsedFolder = new File(folder, PARSED);
		parsedFolder.mkdir();

		// Run through the files, outputting each relation to its individual
		// file
		for (File f : folder.listFiles()) {
			if (f.isDirectory())
				continue;
			BufferedReader in = new BufferedReader(new FileReader(f));
			String line = null;
			int count = 0;
			while ((line = in.readLine()) != null) {
				count++;
				Matcher m = RELATION_PATTERN.matcher(line);
				if (m.matches()) {
					String relation = m.group(1);
					String first = m.group(2);
					String second = m.group(3);

					// Get the writer
					BufferedWriter out = relationFiles.get(relation);
					if (out == null) {
						File outFile = new File(parsedFolder, relation + ".txt");
						outFile.createNewFile();
						out = new BufferedWriter(new FileWriter(outFile));
						relationFiles.put(relation, out);
					}
					out.write(relation + "\t" + first + "\t" + second + "\n");
				}

				if (count % 100000 == 0) {
					System.out.println(count + " processed in " + f);
				}
			}
			in.close();
		}

		for (BufferedWriter writer : relationFiles.values())
			writer.close();
	}

	public static void main(String[] args) {
		try {
			preParseConceptNetFiles(new File(args[0]));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private enum ARFFData {
		RELATION("nominal"),
		ARG1("string"),
		ARG2("string"),
		DISAMB1("string"),
		DISAMB2("string"),
		SEMANTIC_DISTANCE("numeric"),
		SEMANTIC_SIMILARITY("numeric"),
		DEPTH_1("numeric"),
		DEPTH_2("numeric"),
		AV_DEPTH("numeric"),
		COMMON_PARENT("string"),
		PARENT_DEPTH("numeric"),
		RELATION_RELIABILITY("numeric"),
		RELATION_DISJOINT("numeric"),
		RELATION_CONJOINT("numeric"),
		RELATION_UNKNOWN("numeric"),
		INTRA_RELATION_RELIABILITY("numeric"),
		INTRA_RELATION_DISJOINT("numeric"),
		INTRA_RELATION_CONJOINT("numeric"),
		INTRA_RELATION_UNKNOWN("numeric"),
		CLASS("{ disjoint, conjoint }");

		private String type_;

		private ARFFData(String datatype) {
			type_ = datatype;
		}

		public String getType() {
			return type_;
		}
	}

	/**
	 * Each process handles a separate relation file
	 *
	 * @author Sam Sarjant
	 */
	private class ProcessRelationFile implements Runnable {
		private File file_;
		private RelationData relData_;
		private int numLines_;
		private Collection<String[]> arffInstances_;

		public ProcessRelationFile(File file) {
			file_ = file;
			try {
				LineNumberReader lnr = new LineNumberReader(
						new FileReader(file));
				lnr.skip(Long.MAX_VALUE);
				numLines_ = lnr.getLineNumber();
				// Finally, the LineNumberReader object should be closed to
				// prevent resource leak
				lnr.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			relData_ = new RelationData(file.getName().split("\\.")[0]);
			arffInstances_ = new ArrayList<>();
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
					// TODO for (Node isa : CommonQuery.MINISA.runQuery(dag_,
					// node))
					// collections.add((DAGNode) isa);
				}
			}

			// Remove shallow nodes and materials
			collections.retainAll(ptChildren_);
			// DAGNode material = dag_
			// .findDAGNode("CommonSubstances-Material-Topic");
			// for (Iterator<DAGNode> iter = collections.iterator(); iter
			// .hasNext();) {
			// DAGNode node = iter.next();
			// // Remove instances of material
			// if (queryModule_.prove(false, CommonConcepts.ISA.getNode(dag_),
			// node, material) == QueryResult.TRUE) {
			// iter.remove();
			// continue;
			// }
			// }

			return collections;
		}

		/**
		 * Processes a potentially disjoint pair for a given relation.
		 *
		 * @param relation
		 *            The relation for the pair.
		 * @param first
		 *            The first string argument.
		 * @param second
		 *            The second string argument.
		 * @param pair
		 *            The ordered args of the relation.
		 */
		private void processPair(String relation, String first, String second,
				Pair<DAGNode, DAGNode> pair) {
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
			relData_.incrementRelation(flag);

			// Calculate common parents
			Collection<DAGNode> common = calculateCommon(pair.objA_, pair.objB_);
			for (DAGNode parent : common)
				relData_.addIntraParent(parent, flag);

			// Note unknown pairs for later disjoint edge creation
			if (flag == UNKNOWN) {
				UnknownPair up = new UnknownPair(first, pair.objA_, second,
						pair.objB_, common);
				relData_.unknownPairs_.add(up);
			}

			recordARFFData(relation, first, second, pair, common, flag);
		}

		protected void processLine(String line) {
			String[] split = line.split("\t");
			String relation = split[0];
			String first = split[1];
			String second = split[2];

			// Disambiguate nodes
			Collection<DAGNode> firstNodes = disambiguate(first, true);
			if (firstNodes.isEmpty()) {
				return;
			}
			Collection<DAGNode> secondNodes = disambiguate(second, true);
			if (secondNodes.isEmpty()) {
				return;
			}

			// Find left and right disjoint candidates
			Collection<DAGNode> firstDisjoint = asCollections(firstNodes);
			Collection<DAGNode> secondDisjoint = asCollections(secondNodes);

			// For every disjoint pair
			for (DAGNode firstNode : firstDisjoint) {
				for (DAGNode secondNode : secondDisjoint) {
					Pair<DAGNode, DAGNode> pair = null;
					int compare = firstNode.getName().compareTo(
							secondNode.getName());
					if (compare < 0) {
						pair = new Pair<DAGNode, DAGNode>(firstNode, secondNode);
						processPair(relation, first, second, pair);
					} else if (compare > 0) {
						pair = new Pair<DAGNode, DAGNode>(secondNode, firstNode);
						processPair(relation, second, first, pair);
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

				int count = 0;
				while ((line = in.readLine()) != null) {
					processLine(line);
					count++;
					if (count % 10000 == 0) {
						float percent = 100f * count / numLines_;
						System.out.println(count + " processed in " + file_
								+ " (" + percent + "%)");
						System.out.println(relData_);
					}
				}
				System.out.println("Completed processing file '" + file_ + "'");
				in.close();

				// Infer disjointness
				createDisjointness(relData_);

				writeARFFData(relData_);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		private void writeARFFPreamble(BufferedWriter arffWriter)
				throws IOException {
			arffWriter.write("@relation "
					+ relData_.relation_.replaceAll("\\s+", "_")
					+ "_disjoints\n");
			for (ARFFData arffData : ARFFData.values()) {
				arffWriter.write("@attribute '"
						+ arffData.toString().toLowerCase() + "' "
						+ arffData.getType() + "\n");
			}
			arffWriter.write("@data\n");
		}

		private void writeARFFData(RelationData rData) throws IOException {
			if (arffInstances_.isEmpty() || rData.counts_[UNKNOWN] == 0)
				return;
			File file = new File(rData.relation_ + ARFF_OUT);
			file.createNewFile();
			BufferedWriter arffWriter = new BufferedWriter(new FileWriter(file));
			writeARFFPreamble(arffWriter);

			for (String[] data : arffInstances_) {
				// Add the relation and intra-relation reliability.
				data[ARFFData.RELATION_DISJOINT.ordinal()] = rData.counts_[DISJOINT]
						+ "";
				data[ARFFData.RELATION_CONJOINT.ordinal()] = rData.counts_[CONJOINT]
						+ "";
				data[ARFFData.RELATION_UNKNOWN.ordinal()] = rData.counts_[UNKNOWN]
						+ "";
				data[ARFFData.RELATION_RELIABILITY.ordinal()] = (1f * rData.counts_[DISJOINT] / (rData.counts_[DISJOINT] + rData.counts_[CONJOINT]))
						+ "";

				int[] counts = rData.intraNodeCounts_
						.get(UtilityMethods.shrinkString(
								data[ARFFData.COMMON_PARENT.ordinal()], 1));
				if (counts != null) {
					data[ARFFData.INTRA_RELATION_RELIABILITY.ordinal()] = (1f * counts[DISJOINT] / (counts[DISJOINT] + counts[CONJOINT]))
							+ "";
					data[ARFFData.INTRA_RELATION_DISJOINT.ordinal()] = counts[DISJOINT]
							+ "";
					data[ARFFData.INTRA_RELATION_CONJOINT.ordinal()] = counts[CONJOINT]
							+ "";
					data[ARFFData.INTRA_RELATION_UNKNOWN.ordinal()] = counts[UNKNOWN]
							+ "";
				} else {
					data[ARFFData.INTRA_RELATION_RELIABILITY.ordinal()] = "";
					data[ARFFData.INTRA_RELATION_DISJOINT.ordinal()] = "";
					data[ARFFData.INTRA_RELATION_CONJOINT.ordinal()] = "";
					data[ARFFData.INTRA_RELATION_UNKNOWN.ordinal()] = "";
				}
				arffWriter.write(StringUtils.join(data, ',') + "\n");
			}

			arffWriter.close();
		}

		/**
		 * Records data about the disambiguated edges. This is for use in a
		 * potential Machine Learning application later.
		 *
		 * @param relation
		 *            The relation of the triple.
		 * @param first
		 *            The first word.
		 * @param second
		 *            The second word.
		 * @param pair
		 *            The disambiguated pair.
		 * @param common
		 *            The common parent(s) for the pair
		 * @param flag
		 *            The result of the proof.
		 */
		private void recordARFFData(String relation, String first,
				String second, Pair<DAGNode, DAGNode> pair,
				Collection<DAGNode> common, int flag) {
			// Init modules
			SemanticSimilarityModule ssm = (SemanticSimilarityModule) dag_
					.getModule(SemanticSimilarityModule.class);

			String[] data = new String[ARFFData.values().length];
			// Write string stuff
			data[ARFFData.RELATION.ordinal()] = relation;
			data[ARFFData.ARG1.ordinal()] = "'"
					+ first.replaceAll("'", "\\\\'") + "'";
			data[ARFFData.ARG2.ordinal()] = "'"
					+ second.replaceAll("'", "\\\\'") + "'";
			data[ARFFData.DISAMB1.ordinal()] = "'"
					+ pair.objA_.getIdentifier(true).replaceAll("'", "\\\\'")
					+ "'";
			data[ARFFData.DISAMB2.ordinal()] = "'"
					+ pair.objB_.getIdentifier(true).replaceAll("'", "\\\\'")
					+ "'";
			int depthA = Integer.parseInt(pair.objA_
					.getProperty(DepthModule.DEPTH_PROPERTY));
			data[ARFFData.DEPTH_1.ordinal()] = depthA + "";
			int depthB = Integer.parseInt(pair.objB_
					.getProperty(DepthModule.DEPTH_PROPERTY));
			data[ARFFData.DEPTH_2.ordinal()] = depthB + "";
			data[ARFFData.AV_DEPTH.ordinal()] = ((depthA + depthB) / 2f) + "";

			if (flag == DISJOINT)
				data[ARFFData.CLASS.ordinal()] = "disjoint";
			else if (flag == CONJOINT)
				data[ARFFData.CLASS.ordinal()] = "conjoint";
			else
				data[ARFFData.CLASS.ordinal()] = "unknown";

			// //// Measures //////
			// Semantic similarity
			data[ARFFData.SEMANTIC_SIMILARITY.ordinal()] = ""
					+ ssm.semanticSimilarity(pair.objA_, pair.objB_);

			// TODO Currently using all parents. Also try with min-parents
			Collection<? extends Node> minParents = CommonQuery
					.minGeneralFilter(common, dag_);
			for (Node commonParent : minParents) {
				// Common parent
				String[] cloneData = Arrays.copyOf(data, data.length);
				cloneData[ARFFData.COMMON_PARENT.ordinal()] = "'"
						+ commonParent.getIdentifier(true).replaceAll("'",
								"\\\\'") + "'";
				int parentDepth = Integer.parseInt(((DAGNode) commonParent)
						.getProperty(DepthModule.DEPTH_PROPERTY));
				cloneData[ARFFData.PARENT_DEPTH.ordinal()] = parentDepth + "";

				cloneData[ARFFData.SEMANTIC_DISTANCE.ordinal()] = (Math
						.abs(depthA - parentDepth) + Math.abs(depthB
						- parentDepth))
						+ "";

				arffInstances_.add(cloneData);
			}
		}
	}

	public enum RelationColumn {
		DISJOINT_EDGE,
		RELATION,
		ARG1TEXT,
		ARG2TEXT,
		REL_DISJ,
		REL_CONJ,
		REL_UNKN,
		REL_SIG,
		REL_RELIA,
		ARG1_CONJ_COUNT,
		ARG2_CONJ_COUNT,
		COMMON_PARENTS,
		AV_PARENT_CONJ_COUNTS,
		PARENT_RELI,
		AV_PARENT_RELI;
	}

	private class RelationData {

		public int[] counts_ = new int[3];
		public Map<String, int[]> intraNodeCounts_;
		public String relation_;
		public Collection<UnknownPair> unknownPairs_;

		public RelationData(String relation) {
			relation_ = relation;
			intraNodeCounts_ = new ConcurrentHashMap<>();
			unknownPairs_ = new ArrayList<>();
		}

		public void addIntraParent(DAGNode parent, int flag) {
			String parentStr = parent.getIdentifier(true);
			int[] counts = intraNodeCounts_.get(parentStr);
			if (!intraNodeCounts_.containsKey(parentStr)) {
				counts = new int[3];
				intraNodeCounts_.put(parentStr, counts);
			}
			counts[flag]++;
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

	private class UnknownPair {
		private String argA_;
		private String argB_;
		private DAGNode nodeA_;
		private DAGNode nodeB_;
		private Collection<DAGNode> commonParents_;

		public UnknownPair(String argA, DAGNode nodeA, String argB,
				DAGNode nodeB, Collection<DAGNode> commonParents) {
			argA_ = argA;
			argB_ = argB;
			nodeA_ = nodeA;
			nodeB_ = nodeB;
			commonParents_ = commonParents;
		}
	}
}
