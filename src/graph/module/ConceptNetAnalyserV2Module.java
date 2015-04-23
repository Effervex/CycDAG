package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.EdgeModifier;
import graph.core.Node;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.QueryResult;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Serializable;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
	private static final String ARFF_OUT = "DisjointData.arff";
	private static final File DISJOINT_OUT = new File("createdDisjoints.txt");
	private static final String PARSED = "parsed";

	private static final Pattern RELATION_PATTERN = Pattern
			.compile("/a/\\[.+?\\]\t/r/(\\S+)\t/c/[a-z]+/([^ \t/]+)\\S*\t/c/[a-z]+/([^ \t/]+)\t\\S+\t[\\d.]+.+?$");
	private static final long serialVersionUID = 1L;
	public static final int CONJOINT = 1;
	public static final int DISJOINT = 0;
	public static final int UNKNOWN = 2;

	private transient NodeAliasModule aliasModule_;

	/** The pairs already processed. */
	private transient MultiMap<String, Pair<DAGNode, DAGNode>> explored_;
	/** The unresolved nodes that have isa parents */
	private transient MultiMap<String, DAGNode> isaEdges_;
	/** The children of partially tangible. */
	private transient Collection<Node> ptChildren_;

	/** The QueryModule access. */
	private transient QueryModule queryModule_;
	private Map<String, RelationData> relDataMap_;
	private transient SemanticSimilarityModule semSimModule_;

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
		Collection<DAGNode> nodes = new HashSet<>();
		// Only keep collections
		for (Node node : aliasModule_.findNodes(conceptName, false, true)) {
			// If the node is an individual, use the MIN-ISA
			if (queryModule_.prove(false, CommonConcepts.ISA.getNode(dag_),
					node, CommonConcepts.INDIVIDUAL.getNode(dag_)) == QueryResult.TRUE) {
				for (Node n : CommonQuery.MINISA.runQuery(dag_, node))
					nodes.add((DAGNode) n);
			}
			nodes.add((DAGNode) node);
		}

		// Add isa information too if we have it.
//		if (useIsas) {
//			Collection<DAGNode> isas = isaEdges_.get(conceptName);
//			if (isas != null)
//				nodes.addAll(isas);
//		}

		nodes.retainAll(ptChildren_);
		return nodes;
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

	@Override
	public Boolean execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		aliasModule_ = (NodeAliasModule) dag_.getModule(NodeAliasModule.class);
		explored_ = MultiMap.createConcurrentHashSetMultiMap();
		// Do not include PartiallyTangible itself.
		// ptChildren_.remove(CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));
		relDataMap_ = new HashMap<>();

		if (args.length == 0)
			return false;
		File dataFolder = new File(args[0].toString() + File.separator + PARSED);
		if (!dataFolder.exists() || dataFolder.listFiles().length == 0)
			try {
				preParseConceptNetFiles(new File(args[0].toString()));
			} catch (IOException e) {
				e.printStackTrace();
			}

		try {
			DISJOINT_OUT.createNewFile();
			BufferedWriter out = new BufferedWriter(
					new FileWriter(DISJOINT_OUT));
			out.write(StringUtils.join(RelationColumn.values(), '\t') + '\n');
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}

//		readIsaData(dataFolder);
		countDisjointness(dataFolder);

		// Clean up
		explored_.clear();
		ptChildren_.clear();

		return true;
	}

	/**
	 * Calculates a bunch of ARFF data for a pair of possible disjoint concepts.
	 * These concepts were originally linked by a relation. They may have also
	 * calculated their common parents already (if not these will be
	 * calculated).
	 *
	 * @param relation
	 *            The relation between the concepts.
	 * @param conceptA
	 *            The first concept.
	 * @param conceptB
	 *            The second concept.
	 * @param commonParents
	 *            The possibly null precalculated set of common parents for the
	 *            two concepts.
	 * @param calculateClass
	 *            If the ontology should be queried if the two are already
	 *            disjoint.
	 * @return The ARFF data representing the pair of relationed concepts.
	 */
	public String[][] getARFFData(String relation, DAGNode conceptA,
			DAGNode conceptB, Collection<DAGNode> commonParents,
			boolean calculateClass) {
		String[] output = new String[ARFFData.values().length];
		Arrays.fill(output, "");
		if (relation != null)
			output[ARFFData.RELATION.ordinal()] = relation;
		output[ARFFData.ARG1.ordinal()] = "'arg1'";
		output[ARFFData.ARG2.ordinal()] = "'arg2'";
		output[ARFFData.DISAMB1.ordinal()] = "'"
				+ conceptA.getIdentifier(true).replaceAll("'", "\\\\'") + "'";
		output[ARFFData.DISAMB2.ordinal()] = "'"
				+ conceptB.getIdentifier(true).replaceAll("'", "\\\\'") + "'";
		// Depth
		int depthA = Integer.parseInt(conceptA
				.getProperty(DepthModule.DEPTH_PROPERTY));
		output[ARFFData.DEPTH_1.ordinal()] = depthA + "";
		int depthB = Integer.parseInt(conceptB
				.getProperty(DepthModule.DEPTH_PROPERTY));
		output[ARFFData.DEPTH_2.ordinal()] = depthB + "";
		output[ARFFData.AV_DEPTH.ordinal()] = ((depthA + depthB) / 2f) + "";

		// Impact
		double impactA = Math.log(CommonQuery.SPECS.runQuery(dag_, conceptA)
				.size());
		output[ARFFData.IMPACT_A.ordinal()] = impactA + "";
		double impactB = Math.log(CommonQuery.SPECS.runQuery(dag_, conceptB)
				.size());
		output[ARFFData.IMPACT_B.ordinal()] = impactB + "";
		output[ARFFData.IMPACT_AB.ordinal()] = (impactA + impactB) + "";

		// Class
		output[ARFFData.CLASS.ordinal()] = "?";
		if (calculateClass) {
			QueryObject qo = new QueryObject(true, false, QueryResult.ALL,
					CommonConcepts.DISJOINTWITH.getNode(dag_), conceptA,
					conceptB);
			queryModule_.executeQuery(qo);
			if (qo.getResultState() == QueryResult.TRUE)
				output[ARFFData.CLASS.ordinal()] = "disjoint";
			else if (qo.getResultState() == QueryResult.FALSE)
				output[ARFFData.CLASS.ordinal()] = "conjoint";
		}
		// Semantic similarity
		output[ARFFData.SEMANTIC_SIMILARITY.ordinal()] = ""
				+ semSimModule_.semanticSimilarity(conceptA, conceptB);

		// Add the relation and intra-relation reliability.
		RelationData rData = null;
		if (relation != null) {
			rData = relDataMap_.get(relation);
			output[ARFFData.RELATION_DISJOINT.ordinal()] = rData.counts_[DISJOINT]
					+ "";
			output[ARFFData.RELATION_CONJOINT.ordinal()] = rData.counts_[CONJOINT]
					+ "";
			output[ARFFData.RELATION_UNKNOWN.ordinal()] = rData.counts_[UNKNOWN]
					+ "";
			output[ARFFData.RELATION_RELIABILITY.ordinal()] = (1f * (rData.counts_[DISJOINT] + 1) / (rData.counts_[DISJOINT]
					+ rData.counts_[CONJOINT] + 2))
					+ "";
		}

		// //// Parent Measures //////

		// For every parent
		if (commonParents == null)
			commonParents = calculateCommon(conceptA, conceptB);
		Collection<? extends Node> minParents = CommonQuery.minGeneralFilter(
				commonParents, dag_);
		String[][] results = new String[minParents.size()][];
		int i = 0;
		for (Node commonParent : minParents) {
			// Common parent
			String[] cloneData = Arrays.copyOf(output, output.length);
			cloneData[ARFFData.COMMON_PARENT.ordinal()] = "'"
					+ commonParent.getIdentifier(true).replaceAll("'", "\\\\'")
					+ "'";
			int parentDepth = Integer.parseInt(((DAGNode) commonParent)
					.getProperty(DepthModule.DEPTH_PROPERTY));
			cloneData[ARFFData.PARENT_DEPTH.ordinal()] = parentDepth + "";

			cloneData[ARFFData.SEMANTIC_DISTANCE.ordinal()] = (Math.abs(depthA
					- parentDepth) + Math.abs(depthB - parentDepth))
					+ "";

			if (rData != null) {
				int[] counts = rData.intraNodeCounts_.get(commonParent
						.getIdentifier(true));
				if (counts != null) {
					cloneData[ARFFData.INTRA_RELATION_RELIABILITY.ordinal()] = (1f * (counts[DISJOINT] + 1) / (counts[DISJOINT]
							+ counts[CONJOINT] + 2))
							+ "";
					cloneData[ARFFData.INTRA_RELATION_DISJOINT.ordinal()] = counts[DISJOINT]
							+ "";
					cloneData[ARFFData.INTRA_RELATION_CONJOINT.ordinal()] = counts[CONJOINT]
							+ "";
					cloneData[ARFFData.INTRA_RELATION_UNKNOWN.ordinal()] = counts[UNKNOWN]
							+ "";
				}
			}

			results[i++] = cloneData;
		}
		return results;
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
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);
		queryModule_ = (QueryModule) directedAcyclicGraph
				.getModule(QueryModule.class);
		semSimModule_ = (SemanticSimilarityModule) directedAcyclicGraph
				.getModule(SemanticSimilarityModule.class);
		ptChildren_ = CommonQuery.SPECS.runQuery(dag_,
				CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		if (edge.getNodes()[0].equals(CommonConcepts.GENLS.getNode(dag_)))
			ptChildren_ = CommonQuery.SPECS.runQuery(dag_,
					CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));
		return super.addEdge(edge);
	}

	@Override
	public boolean removeEdge(DAGEdge edge) {
		if (edge.getNodes()[0].equals(CommonConcepts.GENLS.getNode(dag_)))
			ptChildren_ = CommonQuery.SPECS.runQuery(dag_,
					CommonConcepts.PARTIALLY_TANGIBLE.getNode(dag_));
		return super.removeEdge(edge);
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return !EdgeModifier.isSpecial(edge, dag_);
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}

	public static void main(String[] args) {
		try {
			preParseConceptNetFiles(new File(args[0]));
		} catch (IOException e) {
			e.printStackTrace();
		}
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

	private class InstanceData {

		public Collection<DAGNode> common_;
		public DAGNode disamA_;
		public DAGNode disamB_;
		public String first_;
		public int flag_;
		public String second_;

		public InstanceData(String first, String second, DAGNode objA,
				DAGNode objB, Collection<DAGNode> common, int flag) {
			first_ = first;
			second_ = second;
			disamA_ = objA;
			disamB_ = objB;
			common_ = common;
			flag_ = flag;
		}
	}

	/**
	 * Each process handles a separate relation file
	 *
	 * @author Sam Sarjant
	 */
	private class ProcessRelationFile implements Runnable {
		private Collection<InstanceData> arffInstances_;
		private File file_;
		private int numLines_;
		private RelationData relData_;

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
			synchronized (this) {
				relDataMap_.put(relData_.relation_, relData_);
			}

			arffInstances_ = new ArrayList<>();
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

			recordARFFData(relation, first, second, pair, common, flag);
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
			InstanceData id = new InstanceData(first, second, pair.objA_,
					pair.objB_, common, flag);
			arffInstances_.add(id);
		}

		private void writeARFFData(RelationData rData) throws IOException {
			if (arffInstances_.isEmpty() || rData.counts_[UNKNOWN] == 0)
				return;
			File file = new File(rData.relation_ + ARFF_OUT);
			file.createNewFile();
			BufferedWriter arffWriter = new BufferedWriter(new FileWriter(file));
			writeARFFPreamble(arffWriter);

			// For every stored instance, calculate the instance data
			for (InstanceData instData : arffInstances_) {
				String[][] data = getARFFData(rData.relation_,
						instData.disamA_, instData.disamB_, instData.common_,
						false);
				for (String[] instance : data) {
					instance[ARFFData.ARG1.ordinal()] = "'"
							+ instData.first_.replaceAll("'", "\\\\'") + "'";
					instance[ARFFData.ARG2.ordinal()] = "'"
							+ instData.second_.replaceAll("'", "\\\\'") + "'";
					if (instData.flag_ == DISJOINT)
						instance[ARFFData.CLASS.ordinal()] = "disjoint";
					else if (instData.flag_ == CONJOINT)
						instance[ARFFData.CLASS.ordinal()] = "conjoint";
					else
						instance[ARFFData.CLASS.ordinal()] = "?";

					arffWriter.write(StringUtils.join(instance, ',') + "\n");
				}
			}

			arffWriter.close();
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

			// For every disjoint pair
			for (DAGNode firstNode : firstNodes) {
				for (DAGNode secondNode : secondNodes) {
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
				// createDisjointness(relData_);

				writeARFFData(relData_);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private class RelationData implements Serializable {
		private static final long serialVersionUID = 1L;
		public int[] counts_ = new int[3];
		public Map<String, int[]> intraNodeCounts_;
		public String relation_;

		public RelationData(String relation) {
			relation_ = relation;
			intraNodeCounts_ = new ConcurrentHashMap<>();
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

	public enum RelationColumn {
		ARG1_CONJ_COUNT,
		ARG1TEXT,
		ARG2_CONJ_COUNT,
		ARG2TEXT,
		AV_PARENT_CONJ_COUNTS,
		AV_PARENT_RELI,
		COMMON_PARENTS,
		DISJOINT_EDGE,
		PARENT_RELI,
		REL_CONJ,
		REL_DISJ,
		REL_RELIA,
		REL_SIG,
		REL_UNKN,
		RELATION;
	}
}
