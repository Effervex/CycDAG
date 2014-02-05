/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Sam Sarjant - initial API and implementation
 ******************************************************************************/
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import util.BooleanFlags;
import util.UtilityMethods;

public class CycDAG extends DirectedAcyclicGraph {
	private static final Pattern CONCEPT_PATTERN = Pattern
			.compile("\\[?#\\$([\\w-:]+[\\w])\\]?");

	private static final Node CYC_IMPORT = new StringNode("CYCImport");

	private static final Node FORWARD_CHAIN = new StringNode("ForwardChain");

	private static final Pattern SUBL_PATTERN = Pattern.compile("[:A-Z].+");

	public static final String MICROTHEORY = "MT";

	private static final Pattern CODE_PATTERN = Pattern
			.compile("<code>.+?</code>");

	private static final Pattern UNCODED_PATTERN = Pattern
			.compile("(?<!<code>)\\(#\\$\\S+( ((#\\$\\S+)|[^a-z\\s]+))+\\)");

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
			CommonConcepts argResult, String microtheory,
			Collection<Edge> forwardEdges, boolean ephemeral) {
		Collection<Substitution> constraints = querier_.execute(
				argTest.getNode(this), predNode,
				PrimitiveNode.parseNode("" + i), VariableNode.DEFAULT);

		for (Substitution sub : constraints) {
			// Skip 'Thing'
			Node constraint = sub.getSubstitution(VariableNode.DEFAULT);
			if (constraint.equals(CommonConcepts.THING.getNode(this)))
				continue;

			// Special case for StringNodes and PrimitiveNodes
			boolean meetsConstraint = false;
			// Special case for string nodes
			if (testNode instanceof StringNode
					&& argQuery.getNode(this).equals(
							CommonConcepts.ISA.getNode(this)))
				meetsConstraint = querier_.proveIsString(testNode, constraint);
			else if (!meetsConstraint)
				meetsConstraint = querier_.prove(argQuery.getNode(this),
						querier_.getExpanded(testNode), constraint);

			if (!meetsConstraint) {
				if (forwardEdges != null) {
					// Create new edge to meet constraint
					Edge forwardEdge = findOrCreateEdge(FORWARD_CHAIN,
							new Node[] { argQuery.getNode(this), testNode,
									constraint }, microtheory, false,
							ephemeral, true);
					if (forwardEdge != null
							&& !(forwardEdge instanceof ErrorEdge)) {
						forwardEdges.add(forwardEdge);
						continue;
					}
				}
				return false;
			}
		}
		return true;
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

	private boolean isCyclic(Node[] edgeNodes) {
		// Cannot prove non-binary edges
		if (edgeNodes.length != 3)
			return false;

		// Self-referential
		if (edgeNodes[1].equals(edgeNodes[2]))
			return false;

		// Only test DAGNodes
		if (!(edgeNodes[1] instanceof DAGNode)
				&& (edgeNodes[2] instanceof DAGNode)) {
			System.out.println("Non-DAG Edge: " + Arrays.toString(edgeNodes));
			return false;
		}

		// If the edge is not symmetric, but is defined symmetrically
		// TODO Technically not correct, but it works.
		if (!querier_.prove(CommonConcepts.ISA.getNode(this), edgeNodes[0],
				CommonConcepts.SYMMETRIC_BINARY.getNode(this))
				&& querier_.prove(edgeNodes[0], edgeNodes[2], edgeNodes[1]))
			return true;
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
	 * @param microtheory
	 *            The microtheory to assert the edge under.
	 * @param flags
	 *            The flags to propagate.
	 * @return True if the proagated edge is valid.
	 */
	private Edge propagateEdge(Edge edge, Node creator, String microtheory,
			boolean[] flags) {
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
				return findOrCreateEdge(creator, propagatedNodes, microtheory,
						flags);
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

	public Edge findOrCreateEdge(Node creator, Node[] edgeNodes,
			String microtheory, boolean... flags) {
		BooleanFlags bFlags = edgeFlags_.loadFlags(flags);
		QueryModule qm = (QueryModule) getModule(QueryModule.class);

		if (containsVariables(edgeNodes))
			return CycDAGErrorEdge.VARIABLE_NODE;

		// Check if the edge is valid
		if (semanticArgCheck(edgeNodes, microtheory,
				bFlags.getFlag("forceConstraints"), bFlags.getFlag("ephemeral"))) {
			// Add alias info
			if (qm.execute(CommonConcepts.GENLPREDS.getNode(this),
					edgeNodes[0], CommonConcepts.TERM_STRING.getNode(this)) != null) {
				for (int i = 2; i < edgeNodes.length; i++)
					((NodeAliasModule) getModule(NodeAliasModule.class))
							.addAlias((DAGNode) edgeNodes[1],
									edgeNodes[2].getName());
			}

			// Check disjointness (only if no checks)
			if (!noChecks_ && isDisjoint(edgeNodes))
				return CycDAGErrorEdge.DISJOINT_EDGE;

			// Check symmetry
			if (!noChecks_ && isCyclic(edgeNodes))
				return CycDAGErrorEdge.CYCLIC_EDGE;

			Edge edge = super.findOrCreateEdge(creator, edgeNodes, flags);
			if (!(edge instanceof ErrorEdge)
					&& ((DAGEdge) edge).getProperty(MICROTHEORY) == null) {
				if (microtheory != null)
					addProperty((DAGObject) edge, MICROTHEORY, microtheory);
				// Propagate subpreds
				Edge propEdge = propagateEdge(edge, creator, microtheory, flags);
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
	public synchronized Edge findOrCreateEdge(Node creator, Node[] edgeNodes,
			boolean... flags) {
		return findOrCreateEdge(creator, edgeNodes, null, flags);
	}

	@Override
	public Node[] parseNodes(String strNodes, Node creator,
			boolean createNodes, boolean dagNodeOnly) {
		// TODO Check this
		return parseNodes(strNodes, creator, createNodes, dagNodeOnly, true);
	}

	public Node[] parseNodes(String strNodes, Node creator,
			boolean createNodes, boolean dagNodeOnly, boolean allowVariables) {
		if (strNodes.startsWith("("))
			strNodes = UtilityMethods.shrinkString(strNodes, 1);
		ArrayList<String> split = UtilityMethods.split(strNodes, ' ');

		Node[] nodes = new Node[split.size()];
		int i = 0;
		for (String arg : split) {
			if (!allowVariables && arg.startsWith("?"))
				return null;
			nodes[i] = findOrCreateNode(arg, creator, createNodes, false,
					dagNodeOnly, allowVariables);

			if (nodes[i] == null)
				return null;
			i++;
		}
		return nodes;
	}

	@Override
	public synchronized Node findOrCreateNode(String nodeStr, Node creator,
			boolean... flags) {
		BooleanFlags bFlags = nodeFlags_.loadFlags(flags);
		boolean createNew = bFlags.getFlag("createNew");
		boolean dagNodeOnly = bFlags.getFlag("dagNodeOnly")
				|| nodeStr.startsWith("#$");
		boolean allowVariables = bFlags.getFlag("allowVariables");
		Node node = super.findOrCreateNode(nodeStr, creator, flags);
		if (node != null)
			return node;

		if (nodeStr.startsWith("(")) {
			FunctionIndex functionIndexer = (FunctionIndex) getModule(FunctionIndex.class);
			Node[] subNodes = parseNodes(nodeStr, creator, createNew,
					dagNodeOnly, allowVariables);
			if (subNodes != null
					&& (!createNew || semanticArgCheck(subNodes, null, false,
							bFlags.getFlag("ephemeral")))) {
				nodeLock_.lock();
				try {
					OntologyFunction ontFunc = functionIndexer
							.execute((Object[]) subNodes);
					if (ontFunc == null) {
						ontFunc = new OntologyFunction(this, subNodes);
						if (!ontFunc.isAnonymous() && createNew) {
							boolean result = nodes_.add(ontFunc);
							if (result) {
								if (bFlags.getFlag("ephemeral"))
									addProperty(ontFunc, EPHEMERAL_MARK, "true");
								// Trigger modules
								for (DAGModule<?> module : getModules())
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
	public void initialiseInternal() {
		super.initialiseInternal();
		noChecks_ = true;
		CommonConcepts.initialise(this);
		CommonConcepts.createCommonAssertions(this);
		noChecks_ = false;

		if (this.getNumNodes() <= 100) {
			try {
				noChecks_ = true;

				readAssertionFile(new File("allAssertions.txt"), CYC_IMPORT);

				noChecks_ = false;
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
		while ((edgeStr = reader.readLine()) != null) {
			// Check for multiline comments.
			String[] split = edgeStr.split("\\t");
			if (split.length > 2) {
				System.err.println("Edge has more than one tab field! "
						+ edgeStr);
				System.exit(1);
			}

			try {
				// Remove SUBL edges
				if (containsSubL(split[0])) {
					nullCount++;
					continue;
				}

				Node[] nodes = parseNodes(split[0], creator, true, false, false);
				if (nodes != null) {
					// Comment cleaning
					if (nodes[0].equals(CommonConcepts.COMMENT.getNode(this))) {
						nodes[2] = processComment(nodes);
					}

					String microtheory = (split.length == 2) ? split[1]
							.replaceAll("#\\$", "") : null;
					Edge edge = findOrCreateEdge(CYC_IMPORT, nodes,
							microtheory, false);
					if (edge instanceof ErrorEdge)
						nullCount++;
				} else
					nullCount++;
			} catch (Exception e) {
				System.err.println(edgeStr);
				e.printStackTrace();
			}
		}

		System.out.println("\nNull count: " + nullCount + ", Duplicate count: "
				+ duplicateCount + "\n");

		reader.close();
	}

	private StringNode processComment(Node[] nodes) {
		String comment = nodes[2].toString();
		// Find uncoded expressions
		Matcher m = UNCODED_PATTERN.matcher(comment);
		comment = m.replaceAll("<code>$0</code>");

		// Remove #$ in code tags
		m = CODE_PATTERN.matcher(comment);
		StringBuffer replComment = new StringBuffer();
		int start = 0;
		while (m.find()) {
			int end = m.start();
			replComment.append(comment.substring(start, end));
			replComment.append(m.group().replaceAll("#\\$", ""));
			start = m.end();
		}
		replComment.append(comment.substring(start, comment.length()));

		// Markup #$ tagged non-commented concepts (with depluralisation)
		comment = replComment.toString();
		m = CONCEPT_PATTERN.matcher(comment);
		replComment = new StringBuffer();
		start = 0;
		while (m.find()) {
			int end = m.start();
			replComment.append(comment.substring(start, end));
			String concept = m.group(1);
			if (findDAGNode(concept) == null) {
				if (findDAGNode(concept.substring(0, concept.length() - 1)) != null)
					concept = concept.substring(0, concept.length() - 1);
			}
			replComment.append("[[" + concept + "]]");
			start = m.end();
		}
		replComment.append(comment.substring(start, comment.length()));

		return new StringNode(replComment.toString());
	}

	/**
	 * Checks the arguments of an edge based on the argNIsa/argNGenls defined by
	 * the edge predicate.
	 * 
	 * @param edgeNodes
	 *            The nodes for the edge.
	 * @param microtheory
	 *            The microtheory the edge is being asserted under.
	 * @param forwardChainCreate
	 *            If instantiation of collection types should be forced to meet
	 *            arg constraints (unless disjoint).
	 * @param ephemeral
	 *            If any forward chained edges are ephemeral.
	 * @return True if the edge is semantically well-formed.
	 */
	public boolean semanticArgCheck(Node[] edgeNodes, String microtheory,
			boolean forwardChainCreate, boolean ephemeral) {
		if (noChecks_)
			return true;
		DAGNode predNode = (DAGNode) edgeNodes[0];
		// Check arity
		if (!checkArity(predNode, edgeNodes))
			return false;

		// Forward chain force-create edges
		Collection<Edge> forwardEdges = (forwardChainCreate) ? new ArrayList<Edge>()
				: null;

		// Check each arg
		for (int i = 1; i < edgeNodes.length; i++) {
			if (!singleArgCheck(predNode, i, edgeNodes[i], microtheory,
					forwardEdges, ephemeral)) {
				if (forwardChainCreate) {
					// Unassert the edges
					for (Edge e : forwardEdges)
						removeEdge(e);
				}
				return false;
			}
		}

		return true;
	}

	public boolean singleArgCheck(DAGNode predNode, int i, Node arg) {
		return singleArgCheck(predNode, i, arg, null, null, false);
	}

	/**
	 * Check if the argument for the edge meets the constraints for a single
	 * position.
	 * 
	 * @param predNode
	 *            The predicate to meet.
	 * @param i
	 *            The index of the argument.
	 * @param arg
	 *            The argument in the predicated edge.
	 * @param microtheory
	 *            The microtheory to assert the edge under.
	 * @param forwardEdges
	 *            If not null, new edges may be created to meet the constraints.
	 * @param ephemeral
	 *            If forward edges are to be created as ephemeral.
	 * @return True if the argument meets the constraint.
	 */
	public boolean singleArgCheck(DAGNode predNode, int i, Node arg,
			String microtheory, Collection<Edge> forwardEdges, boolean ephemeral) {
		// argNIsa
		if (!checkSingleArg(predNode, arg, i, CommonConcepts.ARGISA,
				CommonConcepts.ISA, CommonConcepts.RESULT_ISA, microtheory,
				forwardEdges, ephemeral))
			return false;

		// argNGenls
		if (!checkSingleArg(predNode, arg, i, CommonConcepts.ARGGENL,
				CommonConcepts.GENLS, CommonConcepts.RESULT_GENL, microtheory,
				forwardEdges, ephemeral))
			return false;
		return true;
	}

	static {
		nodeFlags_.addFlag("allowVariables", false);

		edgeFlags_.addFlag("forceConstraints", false);
	}
}
