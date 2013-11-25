package graph.core;

import graph.inference.CommonQuery;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.DAGModule;
import graph.module.FunctionIndex;
import graph.module.NodeAliasModule;
import graph.module.QueryModule;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;

import util.UtilityMethods;

public class CycDAG extends DirectedAcyclicGraph {
	public static final String MICROTHEORY = "MT";

	private static final Node CYC_IMPORT = new StringNode("CYCImport");

	private static final Node FORWARD_CHAIN = new StringNode("ForwardChain");

	private static final Pattern SUBL_PATTERN = Pattern.compile("[A-Z].+");

	private transient QueryModule querier_;

	public CycDAG() {
		this(new File("cyc"));
	}

	public CycDAG(File rootDir) {
		super(rootDir);
		querier_ = (QueryModule) getModule(QueryModule.class);
	}

	private boolean checkArity(DAGNode predNode, Node[] edgeNodes) {
		Collection<Substitution> subs = querier_.execute(
				CommonConcepts.ARITY.getNode(this), predNode,
				VariableNode.DEFAULT);
		for (Substitution sub : subs) {
			if (!((PrimitiveNode) sub.getSubstitution(VariableNode.DEFAULT))
					.getName().equals("" + (edgeNodes.length - 1)))
				return false;
		}
		return true;
	}

	private boolean checkSingleArg(DAGNode predNode, Node testNode, int i,
			CommonConcepts argTest, CommonConcepts argQuery,
			CommonConcepts argResult, VariableNode varNode) {
		Collection<Substitution> constraints = querier_.execute(
				argTest.getNode(this), predNode,
				PrimitiveNode.parseNode("" + i), varNode);

		for (Substitution sub : constraints) {
			// Skip 'Thing'
			if (sub.getSubstitution(varNode).equals(
					CommonConcepts.THING.getNode(this)))
				continue;
			if (!querier_.prove(argQuery.getNode(this),
					querier_.getExpanded(testNode),
					sub.getSubstitution(varNode)))
				return false;
		}
		return true;
	}

	private boolean containsVariables(Node[] edgeNodes) {
		if (edgeNodes == null)
			return false;

		for (Node n : edgeNodes) {
			if (n instanceof VariableNode)
				return true;
			else if (n instanceof OntologyFunction
					&& containsVariables(((OntologyFunction) n).getNodes()))
				return true;
		}
		return false;
	}

	private boolean isDisjoint(Node[] edgeNodes) {
		// If not isa/genls, return true
		Collection<Node> existingCols = null;
		if (edgeNodes[0].equals(CommonConcepts.ISA.getNode(this)))
			existingCols = CommonQuery.MINISA.runQuery(this, edgeNodes[1]);
		else if (edgeNodes[0].equals(CommonConcepts.GENLS.getNode(this)))
			existingCols = CommonQuery.MINGENLS.runQuery(this, edgeNodes[1]);
		else
			return false;

		for (Node n : existingCols) {
			if (querier_.prove(CommonConcepts.DISJOINTWITH.getNode(this),
					edgeNodes[2], n))
				return true;
		}
		return false;
	}

	/**
	 * Propagates certain edges to propagatable predicates (i.e. asserts forward
	 * inferred information). If the assertion is valid, returns true.
	 * 
	 * @param edge
	 *            The propagatable edge being asserted.
	 * @param creator
	 *            What creates the new edge.
	 * @return True if the proagated edge is valid.
	 */
	private Edge propagateEdge(Edge edge, Node creator) {
		DAGNode pred = (DAGNode) edge.getNodes()[0];
		// Check if the predicate genlPreds one of the propagatable predicates
		for (PropagatablePredicate pp : PropagatablePredicate.values()) {
			if (pred.equals(pp.getPred().getNode(this)))
				return edge;
			if (querier_.prove(CommonConcepts.GENLPREDS.getNode(this), pred, pp
					.getPred().getNode(this))) {
				// Propagate the assertion
				Node[] propagatedNodes = Arrays.copyOf(edge.getNodes(),
						edge.getNodes().length);
				propagatedNodes[0] = pp.getPred().getNode(this);
				return findOrCreateEdge(creator, false, propagatedNodes);
			}
		}
		return edge;
	}

	@Override
	protected String preParseNode(String nodeStr, Node creator,
			boolean createNew, boolean dagNodeOnly) {
		nodeStr = super.preParseNode(nodeStr, creator, createNew, dagNodeOnly);
		if (nodeStr.startsWith("#$")) {
			nodeStr = nodeStr.substring(2);
		} else if (CYC_IMPORT.equals(creator)) {
			if (nodeStr.startsWith("?"))
				return null;
			if (PrimitiveNode.parseNode(nodeStr) != null)
				nodeStr = "'" + nodeStr;
		}
		return nodeStr;
	}

	@Override
	public void clear() {
		super.clear();
		for (CommonConcepts cc : CommonConcepts.values())
			cc.clearNode();
	}

	@Override
	public synchronized Edge findOrCreateEdge(Node creator,
			boolean createNodes, Node... edgeNodes) {
		QueryModule qm = (QueryModule) getModule(QueryModule.class);

		if (containsVariables(edgeNodes))
			return CycDAGErrorEdge.VARIABLE_NODE;

		// Check if the edge is valid
		if (semanticArgCheck(false, edgeNodes)) {
			// Add alias info
			if (qm.execute(CommonConcepts.GENLPREDS.getNode(this),
					edgeNodes[0], CommonConcepts.TERM_STRING.getNode(this)) != null) {
				for (int i = 2; i < edgeNodes.length; i++)
					((NodeAliasModule) getModule(NodeAliasModule.class))
							.addAlias((DAGNode) edgeNodes[1],
									edgeNodes[2].getName());
			}

			// Check disjointness
			if (!noChecks_ && isDisjoint(edgeNodes))
				return CycDAGErrorEdge.DISJOINT_EDGE;

			Edge edge = super.findOrCreateEdge(creator, true, edgeNodes);
			if (!(edge instanceof ErrorEdge)) {
				// Propagate subpreds
				Edge propEdge = propagateEdge(edge, creator);
				if (propEdge instanceof ErrorEdge) {
					removeEdge(edge);
					return propEdge;
				}
			}

			return edge;
		}
		return CycDAGErrorEdge.SEMANTIC_CONFLICT;
	}

	@Override
	public synchronized Node findOrCreateNode(String nodeStr, Node creator,
			boolean createNew, boolean dagNodeOnly, boolean allowVariables) {
		dagNodeOnly |= nodeStr.startsWith("#$");
		Node node = super.findOrCreateNode(nodeStr, creator, createNew,
				dagNodeOnly, allowVariables);
		if (node != null)
			return node;

		if (nodeStr.startsWith("(")) {
			FunctionIndex functionIndexer = (FunctionIndex) getModule(FunctionIndex.class);
			Node[] subNodes = parseNodes(nodeStr, creator, createNew,
					allowVariables);
			if (subNodes != null
					&& (!createNew || semanticArgCheck(false, subNodes))) {
				nodeLock_.lock();
				try {
					OntologyFunction ontFunc = functionIndexer
							.execute((Object[]) subNodes);
					if (ontFunc == null) {
						ontFunc = new OntologyFunction(subNodes);
						if (!ontFunc.isAnonymous(this) && createNew) {
							boolean result = nodes_.add(ontFunc);
							if (result) {
								// Trigger modules
								for (DAGModule<?> module : getModules()
										.values())
									module.addNode(ontFunc);
							}
						}
					}
					return ontFunc;
				} finally {
					nodeLock_.unlock();
				}
			}

		} else if (!dagNodeOnly && nodeStr.startsWith("?")) {
			return new VariableNode(nodeStr);
		}
		return null;
	}

	public Edge getRandomEdge(boolean allowFunction) {
		Edge e = null;
		boolean containsFunction = false;
		do {
			e = super.getRandomEdge();

			containsFunction = false;
			for (Node n : e.getNodes()) {
				if (n instanceof OntologyFunction) {
					containsFunction = true;
					break;
				}
			}
		} while (!allowFunction && containsFunction);
		return e;
	}

	public Node getRandomNode(boolean allowFunction) {
		Node n = null;
		do {
			n = super.getRandomNode();
		} while (!allowFunction && n instanceof OntologyFunction);
		return n;
	}

	@Override
	public void initialise(String[] args) {
		super.initialise(args);
		noChecks_ = true;
		CommonConcepts.initialise(this);
		CommonConcepts.createCommonAssertions(this);
		noChecks_ = false;

		if (this.getNumNodes() <= 100) {
			try {
				// ((FileBasedTrieCollection) nodes_)
				//
				// .setSerialisationMechanism(FileBasedTrieCollection.SerialisationMechanism.ADDITIVE_THREAD);
				// ((FileBasedTrieCollection) edges_)
				//
				// .setSerialisationMechanism(FileBasedTrieCollection.SerialisationMechanism.ADDITIVE_THREAD);
				noChecks_ = true;

				// readNodeFile(new File("constants.txt"), CYC_IMPORT);
				// readAssertionFile(new File("taxonomic.txt"), CYC_IMPORT);
				// readAssertionFile(new File("disjoint.txt"), CYC_IMPORT);
				// readAssertionFile(new File("other.txt"), CYC_IMPORT);
				readAssertionFile(new File("allAssertions.txt"), CYC_IMPORT);

				noChecks_ = false;
				// ((FileBasedTrieCollection) nodes_)
				//
				// .setSerialisationMechanism(FileBasedTrieCollection.SerialisationMechanism.DEFAULT);
				// ((FileBasedTrieCollection) edges_)
				//
				// .setSerialisationMechanism(FileBasedTrieCollection.SerialisationMechanism.DEFAULT);
			} catch (Exception e) {
				e.printStackTrace();
				System.exit(1);
			}
			saveState();
		}
	}

	public void readAssertionFile(File assertionFile, Node creator)
			throws IOException {
		if (!assertionFile.exists())
			return;

		BufferedReader reader = new BufferedReader(
				new FileReader(assertionFile));
		String edgeStr = null;
		int duplicateCount = 0;
		int nullCount = 0;
		int count = 0;
		while ((edgeStr = reader.readLine()) != null) {
			count++;
			// Check for multiline comments.
			String[] split = edgeStr.split("\\t");
			if (split.length > 2) {
				System.err.println("Edge has more than one tab field! "
						+ edgeStr);
				System.exit(1);
			}

			try {
				if (containsSubL(split[0])) {
					nullCount++;
					continue;
				}
				Node[] nodes = parseNodes(split[0], creator, true, false);
				if (nodes != null) {
					Edge edge = findOrCreateEdge(CYC_IMPORT, false, nodes);
					if (edge instanceof ErrorEdge) {
						nullCount++;
					} else if (split.length == 2)
						addProperty((DAGObject) edge, MICROTHEORY,
								split[1].replaceAll("#\\$", ""));
				} else
					nullCount++;
			} catch (Exception e) {
				System.err.println(edgeStr);
				e.printStackTrace();
			}

			// if (count % 1000 == 0) {
			// saveState();
			// System.exit(0);
			// }
		}

		System.out.println("\nNull count: " + nullCount + ", Duplicate count: "
				+ duplicateCount + "\n");

		reader.close();
	}

	private boolean containsSubL(String edge) {
		edge = edge.replaceAll("\\(|\\)", "");
		ArrayList<String> nodes = UtilityMethods.split(edge, ' ');
		for (String node : nodes) {
			if (SUBL_PATTERN.matcher(node).matches())
				return true;
		}
		return false;
	}

	public void readNodeFile(File nodeFile, Node creator) throws Exception {
		if (!nodeFile.exists())
			return;

		BufferedReader reader = new BufferedReader(new FileReader(nodeFile));
		String nodeStr = null;
		while ((nodeStr = reader.readLine()) != null) {
			findOrCreateNode(nodeStr, CYC_IMPORT, true, true, true);
		}

		reader.close();
	}

	/**
	 * Checks the arguments of an edge based on the argNIsa/argNGenls defined by
	 * the edge predicate.
	 * 
	 * @param forwardChainCreate
	 *            If instantiation of collection types should be forced to meet
	 *            arg constraints (unless disjoint).
	 * @param edgeNodes
	 *            The nodes for the edge.
	 * 
	 * @return True if the edge is semantically well-formed.
	 */
	public boolean semanticArgCheck(boolean forwardChainCreate,
			Node... edgeNodes) {
		if (noChecks_)
			return true;
		DAGNode predNode = (DAGNode) edgeNodes[0];
		// Check arity
		if (!checkArity(predNode, edgeNodes))
			return false;

		// Check args
		Collection<Node[]> forwardEdges = (forwardChainCreate) ? new ArrayList<Node[]>()
				: null;
		for (int i = 1; i < edgeNodes.length; i++) {
			if (!singleArgCheck(predNode, i, edgeNodes[i]))
				return false;
		}

		// Forward chain force-create edges
		if (forwardChainCreate) {
			ArrayList<Long> ids = new ArrayList<>(forwardEdges.size());
			boolean removeEdges = false;
			for (Node[] forwardEdge : forwardEdges) {
				if (findEdge(forwardEdge) == null) {
					Edge edge = findOrCreateEdge(FORWARD_CHAIN, false,
							forwardEdge);
					if (edge instanceof DAGErrorEdge) {
						removeEdges = true;
						break;
					} else
						ids.add(((DAGEdge) edge).getID());
				}
			}

			if (removeEdges) {
				for (Long id : ids)
					removeEdge(id);
				return false;
			}
		}

		return true;
	}

	public boolean singleArgCheck(DAGNode predNode, int i, Node arg) {
		// argNIsa
		if (!checkSingleArg(predNode, arg, i, CommonConcepts.ARGISA,
				CommonConcepts.ISA, CommonConcepts.RESULT_ISA,
				VariableNode.DEFAULT))
			return false;

		// argNGenls
		if (!checkSingleArg(predNode, arg, i, CommonConcepts.ARGGENL,
				CommonConcepts.GENLS, CommonConcepts.RESULT_GENL,
				VariableNode.DEFAULT))
			return false;
		return true;
	}
}
