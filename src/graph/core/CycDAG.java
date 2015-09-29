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
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.DAGModule;
import graph.module.DepthModule;
import graph.module.FunctionIndex;
import graph.module.NodeAliasModule;
import graph.module.QueryModule;
import graph.module.RelatedEdgeModule;
import graph.module.RewriteOfModule;
import graph.module.StringStorageModule;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import util.BooleanFlags;
import util.UtilityMethods;
import util.collection.ValueComparator;

public class CycDAG extends DirectedAcyclicGraph {
	private static final Pattern CODE_PATTERN = Pattern
			.compile("<code>.+?</code>");

	private static final Pattern CONCEPT_PATTERN = Pattern
			.compile("\\[?#\\$([\\w-:]+[\\w])\\]?");

	/** Prefix for functions when writing to non-function CSV. */
	private static final String CSV_FUNCTION_PREFIX = "FUNCTION_";

	/** The Cyc import creator. */
	private static final Node CYC_IMPORT = new StringNode("CYCImport");

	/** The creator for forward chained edges. */
	private static final Node FORWARD_CHAIN = new StringNode("ForwardChain");

	private static final Pattern UNCODED_PATTERN = Pattern
			.compile("(?<!<code>)\\(#\\$\\S+( ((#\\$\\S+)|[^a-z\\s]+))+\\)");

	/** The key for the microtheory property. */
	public static final String MICROTHEORY = "MT";

	private QueryModule querier_;

	private RewriteOfModule rewriteModule_;

	public boolean loadAssertions_;

	public boolean retainingRemovals_;

	public CycDAG() {
		this(new File("cyc"), null, null);
	}

	public CycDAG(File rootDir, File nodeFile, File edgeFile) {
		super(rootDir, nodeFile, edgeFile);
	}

	private DAGErrorEdge checkArity(DAGNode predNode, QueryObject edgeQuery) {
		QueryObject arityQuery = new QueryObject(false,
				edgeQuery.shouldJustify(), QueryResult.TRUE,
				CommonConcepts.ARITY.getNode(this), predNode,
				VariableNode.DEFAULT);
		Collection<Substitution> subs = querier_.executeQuery(arityQuery);
		for (Substitution sub : subs) {
			PrimitiveNode numArgs = (PrimitiveNode) sub
					.getSubstitution(VariableNode.DEFAULT);
			if (!numArgs.getName().equals(
					"" + (edgeQuery.getNodes().length - 1))) {
				edgeQuery.addResult(false, null, arityQuery.getJustification());
				ArityErrorEdge arityError = new ArityErrorEdge(predNode,
						numArgs.toString());
				edgeQuery.setRejectionReason(arityError);
				return arityError;
			}
		}
		return null;
	}

	@Override
	protected synchronized void addModule(DAGModule<?> module) {
		super.addModule(module);
		if (module instanceof QueryModule)
			querier_ = (QueryModule) getModule(QueryModule.class);
		else if (module instanceof RewriteOfModule)
			rewriteModule_ = (RewriteOfModule) getModule(RewriteOfModule.class);
	}

	/**
	 * Checks if an argument meets a given constraint. Optionally creates new
	 * edges to force the constraint.
	 *
	 * @param predNode
	 *            The predicate node defining the constraints.
	 * @param testNode
	 *            The argument being tested.
	 * @param i
	 *            The index of the argument.
	 * @param argTest
	 *            The type of test to apply.
	 * @param argQuery
	 *            The type of query.
	 * @param microtheory
	 *            The microtheory to create new forced edges under.
	 * @param forwardEdges
	 *            If new edges should be created to meet the constraint.
	 * @param ephemeral
	 *            If the edge is ephemeral.
	 * @param edgeQuery
	 *            The original query to add results to.
	 * @return True if the argument meets the predicate constraint (optionally
	 *         after forcing new edges).
	 */
	private boolean checkSingleArg(DAGNode predNode, Node testNode, int i,
			CommonConcepts argTest, CommonConcepts argQuery,
			String microtheory, Collection<Edge> forwardEdges,
			boolean ephemeral, QueryObject edgeQuery) {
		QueryObject constraintQuery = new QueryObject(false,
				edgeQuery.shouldJustify(), QueryResult.TRUE,
				argTest.getNode(this), predNode,
				PrimitiveNode.parseNode("" + i), VariableNode.DEFAULT);
		Collection<Substitution> constraints = querier_
				.executeQuery(constraintQuery);

		for (Substitution sub : constraints) {
			// Skip 'Thing'
			Node constraint = sub.getSubstitution(VariableNode.DEFAULT);
			if (constraint.equals(CommonConcepts.THING.getNode(this)))
				continue;

			// Special case for StringNodes and PrimitiveNodes
			QueryResult meetsConstraint = QueryResult.NIL;
			// Special case for string nodes
			QueryObject proofQuery = null;
			if (testNode instanceof StringNode
					&& argQuery.getNode(this).equals(
							CommonConcepts.ISA.getNode(this)))
				proofQuery = new QueryObject(false, edgeQuery.shouldJustify(),
						QueryResult.ALL, CommonConcepts.GENLS.getNode(this),
						constraint, CommonConcepts.STRING.getNode(this));
			else
				proofQuery = new QueryObject(false, edgeQuery.shouldJustify(),
						QueryResult.ALL, argQuery.getNode(this),
						querier_.getExpanded(testNode), constraint);
			meetsConstraint = querier_.prove(proofQuery);

			if (meetsConstraint == QueryResult.NIL) {
				if (forwardEdges != null) {
					// Create new edge to meet constraint
					Edge forwardEdge = findOrCreateEdge(
							new Node[] { argQuery.getNode(this), testNode,
									constraint }, FORWARD_CHAIN, microtheory,
							true, ephemeral, true);
					if (forwardEdge != null
							&& !(forwardEdge instanceof ErrorEdge)) {
						forwardEdges.add(forwardEdge);
						continue;
					}
				}
				return false;
			} else if (meetsConstraint == QueryResult.FALSE) {
				edgeQuery.getJustification().addAll(
						constraintQuery.getJustification());
				edgeQuery.addResult(false, null, proofQuery.getJustification());
				return false;
			}
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

	private String exportCSVNodes(Node[] nodes,
			Map<OntologyFunction, Integer> functions) throws IOException {
		StringBuilder line = new StringBuilder();
		for (Node n : nodes) {
			String name = n.getIdentifier(true);
			if (n instanceof OntologyFunction) {
				if (functions.containsKey(n))
					name = CSV_FUNCTION_PREFIX + functions.get(n);
				else {
					int num = functions.size();
					functions.put((OntologyFunction) n, num);
					exportCSVNodes(((OntologyFunction) n).getNodes(), functions);
					name = CSV_FUNCTION_PREFIX + num;
				}
			}
			line.append(name + ",");
		}
		line.append("\n");
		return line.toString();
	}

	private CyclicErrorEdge isCyclic(QueryObject edgeQuery, boolean negated) {
		if (negated)
			return null;

		// Cannot prove non-binary edges
		Node[] edgeNodes = edgeQuery.getNodes();
		if (edgeNodes.length != 3)
			return null;

		// Self-referential
		if (edgeNodes[1].equals(edgeNodes[2]))
			return null;

		// Must be monotonic & non-symmetric
		if (querier_.prove(false, CommonConcepts.ISA.getNode(this),
				edgeNodes[0], CommonConcepts.MONOTONIC_PREDICATE.getNode(this)) != QueryResult.TRUE)
			return null;

		// If the edge is not symmetric, but is defined symmetrically
		if (querier_.prove(false, CommonConcepts.ISA.getNode(this),
				edgeNodes[0], CommonConcepts.SYMMETRIC_BINARY.getNode(this)) == QueryResult.TRUE)
			return null;

		QueryObject oppQuery = new QueryObject(false,
				edgeQuery.shouldJustify(), QueryResult.TRUE, edgeNodes[0],
				edgeNodes[2], edgeNodes[1]);
		if (querier_.prove(oppQuery) == QueryResult.TRUE) {
			CyclicErrorEdge cee = new CyclicErrorEdge(edgeNodes);
			edgeQuery.setRejectionReason(cee);
			edgeQuery.addResult(false, null, oppQuery.getJustification());
			return cee;
		}
		return null;
	}

	/**
	 * Checks if the edge nodes can be added or not due to disjointness.
	 * 
	 * @param edgeNodes
	 *            The edge nodes being defined.
	 * @return If the edge created will cause a disjoint contradiction.
	 */
	private DisjointErrorEdge isDisjoint(QueryObject edgeQuery) {
		// If not isa/genls, return true
		Collection<Node> existingCols = null;
		Node[] edgeNodes = edgeQuery.getNodes();
		Node relation = edgeNodes[0];
		if (relation.equals(CommonConcepts.ISA.getNode(this))) {
			existingCols = CommonQuery.MINISA.runQuery(this, edgeNodes[1]);
			if (existingCols == null)
				return null;
		} else if (relation.equals(CommonConcepts.GENLS.getNode(this))) {
			existingCols = new ArrayList<>();
			existingCols.add(edgeNodes[1]);
		} else
			return null;

		// Checks if the existing collections are disjoint with the asserted
		// edge.
		for (Node n : existingCols) {
			QueryObject disjointQuery = new QueryObject(false,
					edgeQuery.shouldJustify(), QueryResult.TRUE,
					CommonConcepts.DISJOINTWITH.getNode(this), n, edgeNodes[2]);
			if (querier_.prove(disjointQuery) == QueryResult.TRUE) {
				DisjointErrorEdge dee = new DisjointErrorEdge(
						(DAGNode) edgeNodes[2], (DAGNode) n, this);
				// Adding in isa justification step
				if (edgeQuery.shouldJustify()
						&& relation.equals(CommonConcepts.ISA.getNode(this)))
					edgeQuery.getJustification().add(
							new Node[] { edgeNodes[0], edgeNodes[1], n });

				// Noting the rest of the result.
				edgeQuery.addResult(false, null,
						disjointQuery.getJustification());
				edgeQuery.setRejectionReason(dee);
				return dee;
			}
		}
		return null;
	}

	private CollectionOrderErrorEdge isInvalidCollectionOrder(
			QueryObject edgeQuery) {
		Node[] edgeNodes = edgeQuery.getNodes();
		Node relation = edgeNodes[0];
		DAGNode isa = CommonConcepts.ISA.getNode(this);

		// If genls
		if (relation.equals(CommonConcepts.GENLS.getNode(this))) {
			VariableNode x = new VariableNode("?X");
			OntologyFunction isaAX = new OntologyFunction(isa, edgeNodes[1], x);
			VariableNode y = new VariableNode("?Y");
			OntologyFunction isaBY = new OntologyFunction(isa, edgeNodes[2], y);
			OntologyFunction isaXOrder = new OntologyFunction(isa, x,
					CommonConcepts.COLLECTION_ORDER.getNode(this));
			OntologyFunction isaYOrder = new OntologyFunction(isa, y,
					CommonConcepts.COLLECTION_ORDER.getNode(this));
			OntologyFunction diffXY = new OntologyFunction(
					CommonConcepts.DIFFERENT.getNode(this), x, y);
			QueryObject qo = new QueryObject(false, edgeQuery.shouldJustify(),
					QueryResult.TRUE, CommonConcepts.AND.getNode(this), isaAX,
					isaBY, isaXOrder, isaYOrder, diffXY);
			QueryResult result = querier_.prove(qo);
			if (result == QueryResult.TRUE) {
				edgeQuery.addResult(false, null, qo.getJustification());
				return new CollectionOrderErrorEdge(edgeNodes);
			}
		}

		// If isa
		else if (relation.equals(isa)) {
			// First check Indiv/Collection
			// (and (isa A Collection) (isa B FirstOrderCollection))
			OntologyFunction isaXCol = new OntologyFunction(isa, edgeNodes[1],
					CommonConcepts.COLLECTION.getNode(this));
			OntologyFunction isaYFOC = new OntologyFunction(isa, edgeNodes[2],
					CommonConcepts.FIRST_ORDER_COLLECTION.getNode(this));
			QueryObject qo = new QueryObject(false, false, QueryResult.TRUE,
					CommonConcepts.AND.getNode(this), isaXCol, isaYFOC);
			QueryResult result = querier_.prove(qo);
			if (result == QueryResult.TRUE) {
				edgeQuery.addResult(false, null, qo.getJustification());
				return new CollectionOrderErrorEdge(edgeNodes);
			}

			// Then check the order is increasing
			// (and (isa A ?X) (isa B ?Y) (isa ?X CollectionOrder) (isa ?Y
			// CollectionOrder))
			VariableNode x = new VariableNode("?X");
			OntologyFunction isaAX = new OntologyFunction(isa, edgeNodes[1], x);
			VariableNode y = new VariableNode("?Y");
			OntologyFunction isaBY = new OntologyFunction(isa, edgeNodes[2], y);
			OntologyFunction isaXOrder = new OntologyFunction(isa, x,
					CommonConcepts.COLLECTION_ORDER.getNode(this));
			OntologyFunction isaYOrder = new OntologyFunction(isa, y,
					CommonConcepts.COLLECTION_ORDER.getNode(this));
			qo = new QueryObject(false, false, QueryResult.TRUE,
					CommonConcepts.AND.getNode(this), isaAX, isaBY, isaXOrder,
					isaYOrder);
			Collection<Substitution> results = querier_.executeQuery(qo);
			// Test for collection order definitions
			if (qo.getResultState() == QueryResult.TRUE) {
				Substitution sub = results.iterator().next();
				if (results.size() != 1
						|| querier_.prove(false, isa, sub.getSubstitution(x),
								sub.getSubstitution(y)) != QueryResult.TRUE) {
					edgeQuery.addResult(false, null, qo.getJustification());
					return new CollectionOrderErrorEdge(edgeNodes);
				}
			}
		}
		return null;
	}

	/**
	 * Checks if the edge nodes create a contradiction by way of negation.
	 *
	 * @param edgeNodes
	 *            The edge nodes being defined.
	 * @param negated
	 *            If the edge nodes are a negated edge.
	 * @return A negated error edge if the edge creates a contradiction.
	 */
	private NegatedErrorEdge isNegated(QueryObject edgeQuery, boolean negated) {
		// Searching directly for negation
		boolean before = edgeQuery.shouldVerify();
		edgeQuery.setVerify(false);
		QueryResult beforeGoal = edgeQuery.getGoalResult();
		edgeQuery.setGoalResult(QueryResult.FALSE);
		if (querier_.prove(edgeQuery) == QueryResult.FALSE) {
			NegatedErrorEdge negatedError = new NegatedErrorEdge(negated,
					edgeQuery.getNodes());
			edgeQuery.setRejectionReason(negatedError);
			return negatedError;
		}
		edgeQuery.setVerify(before);
		edgeQuery.setGoalResult(beforeGoal);
		return null;
	}

	private StringNode processComment(Node[] nodes) {
		String comment = nodes[2].toString();
		// Find uncoded expressions
		Matcher m = UNCODED_PATTERN.matcher(comment);
		comment = m.replaceAll("<code>$0</code>");

		// Remove #$ in code tags
		m = CODE_PATTERN.matcher(comment);
		StringBuilder replComment = new StringBuilder();
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
		replComment = new StringBuilder();
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
			if (querier_.prove(false, CommonConcepts.GENLPREDS.getNode(this),
					pred, pp.getPred().getNode(this)) == QueryResult.TRUE) {
				// Propagate the assertion
				Node[] propagatedNodes = Arrays.copyOf(edge.getNodes(),
						edge.getNodes().length);
				propagatedNodes[0] = pp.getPred().getNode(this);
				return findOrCreateEdge(propagatedNodes, creator, microtheory,
						flags);
			}
		}
		return edge;
	}

	/**
	 * Reifies the function such that it has an ID. This method is used whenever
	 * a new function is directly added as a node, or is used in an edge.
	 *
	 * @param ontFunc
	 *            The function to reify.
	 * @param ephemeral
	 *            If the function is ephemeral.
	 */
	private boolean reifyFunction(OntologyFunction ontFunc, Node creator,
			boolean ephemeral) {
		ontFunc.reify(creator, this);
		boolean result = nodes_.add(ontFunc);
		if (result) {
			if (ephemeral)
				addProperty(ontFunc, EPHEMERAL_MARK, "T");
			// Trigger modules
			for (DAGModule<?> module : getModules()) {
				if (module.supportsNode(ontFunc))
					module.addNode(ontFunc);
			}

			// Reify internal args too
			for (Node n : ontFunc.getNodes()) {
				if (n instanceof OntologyFunction)
					reifyFunction((OntologyFunction) n, creator, ephemeral);
			}
		}
		return result;
	}

	protected void exportRereadable(File file) throws IOException {
		BufferedWriter out = new BufferedWriter(new FileWriter(file));

		for (DAGEdge e : edges_) {
			String mt = e.getProperty(MICROTHEORY);
			if (mt == null)
				mt = "";
			out.write(e.getIdentifier(true) + "\t" + mt + "\n");
		}

		out.close();
	}

	@Override
	protected void exportToCSV(BufferedWriter out, DAGExportFormat format)
			throws IOException {
		Map<OntologyFunction, Integer> functions = new HashMap<>();
		for (DAGEdge e : edges_) {
			Node[] nodes = e.getNodes();
			if (format == DAGExportFormat.CSV_ALL
					|| nodes[0].equals(CommonConcepts.ISA.getNode(this))
					|| nodes[0].equals(CommonConcepts.GENLS.getNode(this))
					|| nodes[0].equals(CommonConcepts.GENLPREDS.getNode(this))
					|| nodes[0].equals(CommonConcepts.GENLMT.getNode(this))) {
				out.write(exportCSVNodes(nodes, functions));
			}
		}

		ValueComparator vc = new ValueComparator(functions);
		SortedMap<OntologyFunction, Integer> sortedFuncs = new TreeMap<>(vc);
		sortedFuncs.putAll(functions);
		for (Map.Entry<OntologyFunction, Integer> entry : sortedFuncs
				.entrySet()) {
			out.write(CSV_FUNCTION_PREFIX + entry.getValue() + ":");
			Node[] functionNodes = entry.getKey().getNodes();
			for (Node n : functionNodes) {
				String name = n.getIdentifier(true);
				if (n instanceof OntologyFunction)
					name = CSV_FUNCTION_PREFIX + functions.get(n);
				out.write(name + ",");
			}
			out.write("\n");
		}
	}

	@Override
	protected SortedSet<DAGEdge> orderedReassertables() {
		Comparator<DAGEdge> depthComparator = new Comparator<DAGEdge>() {
			@Override
			public int compare(DAGEdge o1, DAGEdge o2) {
				String depthStr1 = o1.getProperty(DepthModule.DEPTH_PROPERTY);
				String depthStr2 = o2.getProperty(DepthModule.DEPTH_PROPERTY);
				int result = 0;
				if (depthStr1 != null) {
					if (depthStr2 != null) {
						result = Integer.compare(Integer.parseInt(depthStr1),
								Integer.parseInt(depthStr2));
					} else
						return -1;
				} else if (depthStr2 != null)
					return 1;

				if (result == 0)
					return o1.compareTo(o2);
				return result;
			}
		};
		return new TreeSet<>(depthComparator);
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
	protected void readConfigLine(String variable, String value) {
		super.readConfigLine(variable, value);
		if (variable.equalsIgnoreCase("retainingRemovals")) {
			retainingRemovals_ = value.equalsIgnoreCase("TRUE");
			System.out
					.println("Retaining removal set to " + retainingRemovals_);
		} else if (variable.equalsIgnoreCase("loadAssertions")) {
			loadAssertions_ = value.equalsIgnoreCase("TRUE");
			System.out.println("Load assertions set to " + loadAssertions_);
		}
	}

	@Override
	public void clear() {
		super.clear();
		for (CommonConcepts cc : CommonConcepts.values())
			cc.clearNode();
	}

	@Override
	public void export(File file, DAGExportFormat format) throws IOException {
		super.export(file, format);
		if (format == DAGExportFormat.CUSTOM)
			exportRereadable(file);
	}

	@Override
	public Edge findOrCreateEdge(Node[] edgeNodes, Node creator,
			boolean... flags) {
		return findOrCreateEdge(edgeNodes, creator, null, flags);
	}

	/**
	 * Finds or creates an edge from a set of nodes. The returned edge either
	 * already exists, or is newly created and added.
	 * 
	 * @param edgeNodes
	 *            The nodes of the edge.
	 * @param creator
	 *            The creator of the edge (can be null).
	 * @param microtheory
	 *            The optional microtheory to create the edge under.
	 * @param flags
	 *            The boolean flags to use during edge creation: createNew
	 *            (false), ephemeral (false), forceConstraints (false).
	 * @return The created edge or an ErrorEdge if there was an error.
	 */
	public synchronized Edge findOrCreateEdge(Node[] edgeNodes, Node creator,
			String microtheory, boolean... flags) {
		BooleanFlags bFlags = edgeFlags_.loadFlags(flags);
		boolean createNew = bFlags.getFlag("createNew");
		QueryModule qm = (QueryModule) getModule(QueryModule.class);

		if (containsVariables(edgeNodes))
			return VariableErrorEdge.getInstance();

		if (!noChecks_ && createNew) {
			// Cannot have non-DAG node as 1st argument
			if (!(edgeNodes[1] instanceof DAGNode))
				return new NonDAGNodeErrorEdge(edgeNodes);

			// If negated, convert to alternate format
			boolean negated = edgeNodes[0].equals(CommonConcepts.NOT
					.getNode(this));
			QueryObject qo = null;
			if (negated) {
				qo = new QueryObject(
						((OntologyFunction) edgeNodes[1]).getNodes());
				negated = true;
			} else
				qo = new QueryObject(edgeNodes);
			ErrorEdge ee = verifyEdgeArguments(qo, negated,
					bFlags.getFlag("forceConstraints"), microtheory,
					bFlags.getFlag("ephemeral"));

			if (ee != null)
				return ee;
			else {
				// Check if the negation exists.
				ee = isNegated(qo, negated);
				if (ee != null)
					return ee;
			}
		}

		boolean ephemeral = bFlags.getFlag("ephemeral");
		// Reify any ontology functions
		if (!EdgeModifier.isSpecial(edgeNodes, this)) {
			for (Node n : edgeNodes)
				if (n instanceof OntologyFunction)
					reifyFunction((OntologyFunction) n, creator, ephemeral);
		}

		// Create the edge
		Edge edge = super.findOrCreateEdge(edgeNodes, creator, flags);
		if (edge != null && !(edge instanceof ErrorEdge)
				&& ((DAGEdge) edge).getProperty(MICROTHEORY) == null) {
			// First check if there is a removed edge
			Edge removed = findEdge(CommonConcepts.REMOVED.getNode(this),
					new OntologyFunction(edgeNodes));
			if (removed != null) {
				removeEdge(removed, true);
				copyProperties((DAGEdge) removed, (DAGEdge) edge);
			} else if (microtheory != null)
				addProperty((DAGObject) edge, MICROTHEORY, microtheory);

			// Add alias info
			if (qm.prove(new QueryObject(false, false, QueryResult.TRUE,
					CommonConcepts.GENLPREDS.getNode(this), edgeNodes[0],
					CommonConcepts.TERM_STRING.getNode(this))) == QueryResult.TRUE) {
				addProperty((DAGEdge) edge, NodeAliasModule.ALIAS_PROP, "T");
				NodeAliasModule nam = (NodeAliasModule) getModule(NodeAliasModule.class);
				if (nam.supportsEdge((DAGEdge) edge))
					nam.addEdge((DAGEdge) edge);
			}

			// Propagate subpreds
			Edge propEdge = propagateEdge(edge, creator, microtheory, flags);
			if (propEdge instanceof ErrorEdge) {
				removeEdge(edge, true);
				return propEdge;
			}
		}

		return edge;
	}

	/**
	 * Convenience method for identifying the boolean args.
	 * 
	 * @param edgeNodes
	 *            The nodes of the edge.
	 * @param creator
	 *            The creator of the edge (can be null).
	 * @param microtheory
	 *            The optional microtheory to create the edge under.
	 * @param createNew
	 *            If the edge can be created if not found (default false).
	 * @param ephemeral
	 *            If the edge should be created as ephemeral (default false).
	 * @param forceConstraints
	 *            If unmet constraints should be forcefully created (if
	 *            possible, default false).
	 * @return The created edge or an ErrorEdge if there was an error.
	 */
	public Edge findOrCreateEdge(Node[] edgeNodes, Node creator,
			String microtheory, boolean createNew, boolean ephemeral,
			boolean forceConstraints) {
		return findOrCreateEdge(edgeNodes, creator, microtheory, new boolean[] {
				createNew, ephemeral, forceConstraints });
	}

	/**
	 * Finds or creates a new functionally-defined node.
	 * 
	 * @param checkConstraints
	 *            If the node can be created if not found.
	 * @param ephemeral
	 *            If the node should be ephemeral.
	 * @param creator
	 *            The creator (can be null).
	 * @param args
	 *            The functional args.
	 * @return The OntologyFunction representing the node or null if there is a
	 *         problem.
	 */
	public OntologyFunction findOrCreateFunctionNode(boolean checkConstraints,
			boolean ephemeral, Node creator, Node... args) {
		if (args != null
				&& (!checkConstraints || isSemanticallyValid(new QueryObject(
						args), null, false, ephemeral, false) == null)) {
			FunctionIndex functionIndexer = (FunctionIndex) getModule(FunctionIndex.class);
			nodeLock_.lock();
			try {
				OntologyFunction ontFunc = functionIndexer
						.execute((Object[]) args);
				if (ontFunc == null)
					ontFunc = new OntologyFunction(this, args);
				return ontFunc;
			} finally {
				nodeLock_.unlock();
			}
		}
		return null;
	}

	@Override
	public synchronized Node findOrCreateNode(String nodeStr, Node creator,
			boolean... flags) {
		BooleanFlags bFlags = nodeFlags_.loadFlags(flags);
		boolean createNew = bFlags.getFlag("createNew");
		boolean allowVariables = bFlags.getFlag("allowVariables");
		Node node = super.findOrCreateNode(nodeStr, creator, flags);
		if (node != null) {
			if (rewriteModule_ != null && node instanceof DAGNode)
				return rewriteModule_.getRewrite((DAGNode) node);
			return node;
		}

		if (nodeStr.startsWith("(")) {
			Node[] subNodes = parseNodes(nodeStr, creator, createNew, true,
					allowVariables);
			return findOrCreateFunctionNode(createNew,
					bFlags.getFlag("ephemeral"), creator, subNodes);
		} else if (allowVariables && nodeStr.startsWith("?")) {
			return new VariableNode(nodeStr);
		}
		return null;
	}

	/**
	 * Convenience node find/creation method with identified boolean args.
	 * 
	 * @param nodeStr
	 *            The name of the node to find/create.
	 * @param creator
	 *            The optional creator.
	 * @param createNew
	 *            If a new node should be created if not found.
	 * @param ephemeral
	 *            If the created node should be ephemeral.
	 * @param dagNodeOnly
	 *            If only DAG nodes can be found/created.
	 * @param allowVariables
	 *            If variable nodes are allowed to be created.
	 * @return The found/created node, or null.
	 */
	public Node findOrCreateNode(String nodeStr, Node creator,
			boolean createNew, boolean ephemeral, boolean dagNodeOnly,
			boolean allowVariables) {
		return findOrCreateNode(nodeStr, creator, new boolean[] { createNew,
				ephemeral, dagNodeOnly, allowVariables });
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
		// Register the compressable types
		if (getModule(StringStorageModule.class) != null)
			((StringStorageModule) getModule(StringStorageModule.class))
					.registerCompressableNode(CommonConcepts.COMMENT
							.getNodeName());

		// Need to ensure NodeAlias and RelEdge are initialised
		getModule(NodeAliasModule.class).initialisationComplete(nodes_, edges_,
				false);
		getModule(RelatedEdgeModule.class).initialisationComplete(nodes_,
				edges_, false);

		super.initialiseInternal();

		noChecks_ = true;
		CommonConcepts.initialise(this);
		CommonConcepts.createCommonAssertions(this);
		noChecks_ = false;

		if (this.getNumNodes() <= 100 && loadAssertions_) {
			try {
				readAssertionFile(new File("allAssertions.txt"), CYC_IMPORT);
			} catch (Exception e) {
				e.printStackTrace();
				System.exit(1);
			}
			saveState();
		}
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
	public DAGErrorEdge isSemanticallyValid(QueryObject edgeQuery,
			String microtheory, boolean forwardChainCreate, boolean ephemeral,
			boolean negated) {
		if (noChecks_)
			return null;
		Node[] edgeNodes = edgeQuery.getNodes();
		DAGNode predNode = (DAGNode) edgeNodes[0];
		// Check arity
		DAGErrorEdge error = checkArity(predNode, edgeQuery);
		if (error != null)
			return error;

		// Forward chain force-create edges
		Collection<Edge> forwardEdges = (forwardChainCreate && !negated) ? new ArrayList<Edge>()
				: null;

		// Check each arg
		for (int i = 1; i < edgeNodes.length; i++) {
			if (edgeNodes[i] instanceof VariableNode)
				continue;
			if (!singleArgCheck(predNode, i, edgeNodes[i], microtheory,
					forwardEdges, ephemeral, edgeQuery)) {
				if (forwardChainCreate) {
					// Unassert the edges
					for (Edge e : forwardEdges)
						removeEdge(e, true);
				}
				SemanticArgErrorEdge semanticArgError = new SemanticArgErrorEdge(
						predNode, i, edgeNodes[i]);
				edgeQuery.setRejectionReason(semanticArgError);
				return semanticArgError;
			}
		}

		return null;
	}

	public boolean isValidArgument(DAGNode predNode, int i, Node arg,
			boolean forceConstraint) {
		Collection<Edge> forwardEdges = null;
		if (forceConstraint)
			forwardEdges = new ArrayList<>();
		return singleArgCheck(predNode, i, arg, null, forwardEdges, false,
				new QueryObject());
	}

	@Override
	public Node[] parseNodes(String strNodes, Node creator,
			boolean createNodes, boolean dagNodeOnly) {
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
			if (i != 0)
				dagNodeOnly = false;
			nodes[i] = findOrCreateNode(arg, creator, createNodes, false,
					dagNodeOnly, allowVariables);

			if (nodes[i] == null)
				return null;
			i++;
		}
		return nodes;
	}

	/**
	 * Processes an edge in string form and adds it to the DAG if valid.
	 *
	 * @param edgeStr
	 *            The edge in string form to be added.
	 * @param creator
	 *            The creator of the edge.
	 * @return True if the edge was successfully added.
	 */
	public boolean processEdge(String edgeStr, Node creator) {
		// Check for multiline comments.
		String[] split = edgeStr.split("\\t");
		if (split.length > 2) {
			System.err.println("Edge has more than one tab field! " + edgeStr);
			System.exit(1);
		}

		try {
			Node[] nodes = parseNodes(split[0], creator, true, false, false);
			if (nodes != null) {
				// Comment cleaning
				if (nodes[0].equals(CommonConcepts.COMMENT.getNode(this))) {
					nodes[2] = processComment(nodes);
				}

				String microtheory = (split.length == 2) ? split[1].replaceAll(
						"#\\$", "") : null;
				Edge edge = findOrCreateEdge(nodes, CYC_IMPORT, microtheory,
						true, false, true);
				if (edge instanceof ErrorEdge) {
					return false;
				}
			} else
				return false;
		} catch (Exception e) {
			System.err.println(split[0]);
			e.printStackTrace();
		}
		return true;
	}

	public void readAssertionFile(File assertionFile, Node creator)
			throws IOException {
		if (!assertionFile.exists())
			return;

		noChecks_ = true;
		BufferedReader reader = new BufferedReader(
				new FileReader(assertionFile));
		String edgeStr = null;
		List<String> rejected = new ArrayList<>();
		while ((edgeStr = reader.readLine()) != null) {
			if (!processEdge(edgeStr, creator))
				rejected.add(edgeStr);
		}

		int rejectedSize = rejected.size();
		do {
			rejectedSize = rejected.size();
			Iterator<String> iter = rejected.iterator();
			while (iter.hasNext()) {
				edgeStr = iter.next();
				if (processEdge(edgeStr, creator)) {
					iter.remove();
				}
			}
		} while (rejected.size() != rejectedSize);

		// Output rejected files
		if (!rejected.isEmpty()) {
			File rejectedFile = new File("rejected.txt");
			rejectedFile.createNewFile();
			BufferedWriter rejectedWriter = new BufferedWriter(new FileWriter(
					rejectedFile));
			for (String rejStr : rejected)
				rejectedWriter.write(rejStr + "\n");
			rejectedWriter.close();
		}
		System.out.println("\nNull count: " + rejected.size() + "\n");

		reader.close();
		noChecks_ = false;
	}

	@Override
	public synchronized boolean removeEdge(Edge edge) {
		return removeEdge(edge, true);
	}

	/**
	 * A remove edge method for dealing with the removed special modifier. If
	 * the edge should be removed and not retained, set forceRemove to true.
	 *
	 * @param edge
	 *            The edge to remove
	 * @param forceRemove
	 *            If the edge is completely removed, otherwise it will be
	 *            retained if retain removals is true.
	 * @return If the edge was removed successfully.
	 */
	public synchronized boolean removeEdge(Edge edge, boolean forceRemove) {
		boolean result = super.removeEdge(edge);
		// Cannot remove twice.
		if (!forceRemove && retainingRemovals_ && result
				&& !EdgeModifier.isRemoved(edge, this)) {
			// Add a new 'removed' edge
			Edge removedEdge = findOrCreateEdge(new Node[] {
					CommonConcepts.REMOVED.getNode(this),
					new OntologyFunction(edge.getNodes()) }, null, null, true);
			if (removedEdge instanceof ErrorEdge) {
				// Something went wrong...
				System.err.println("Could not 'remove' edge: "
						+ edge.toString(false));
			} else {
				// Add all the properties
				copyProperties((DAGEdge) edge, (DAGEdge) removedEdge);
			}
		}
		return result;
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
	 * @param edgeQuery
	 *            The original query object.
	 * @return True if the argument meets the constraint.
	 */
	public boolean singleArgCheck(DAGNode predNode, int i, Node arg,
			String microtheory, Collection<Edge> forwardEdges,
			boolean ephemeral, QueryObject edgeQuery) {
		if (arg instanceof VariableNode)
			return true;

		// argNIsa
		if (!checkSingleArg(predNode, arg, i, CommonConcepts.ARGISA,
				CommonConcepts.ISA, microtheory, forwardEdges, ephemeral,
				edgeQuery))
			return false;

		// argNGenls
		if (!checkSingleArg(predNode, arg, i, CommonConcepts.ARGGENL,
				CommonConcepts.GENLS, microtheory, forwardEdges, ephemeral,
				edgeQuery))
			return false;
		return true;
	}

	/**
	 * A series of tests to run an edge through to verify if it is valid. Used
	 * both when creating new edges and when querying the validity of a query
	 * edge.
	 * 
	 * @param negated
	 *            Is the tested edge negated?
	 * @param forceConstraints
	 *            If creating, can edges be created to meet constraints?
	 * @param microtheory
	 *            The microtheory to create new forced edges under.
	 * @param ephemeral
	 *            Are edges created ephemerally?
	 * @param edgeNodes
	 *            The nodes of the edge.
	 * 
	 * @return An error edge or null if no problems.
	 */
	public ErrorEdge verifyEdgeArguments(QueryObject edgeQuery,
			boolean negated, boolean forceConstraints, String microtheory,
			boolean ephemeral) {
		if (noChecks_ || EdgeModifier.isRemoved(edgeQuery.getNode(0), this))
			return null;

		// Check if the edge is semantically valid
		DAGErrorEdge semError = isSemanticallyValid(edgeQuery, microtheory,
				forceConstraints, ephemeral, negated);
		if (semError != null)
			return semError;

		// Skip variables from here out.
		if (edgeQuery.getVariable() != null)
			return null;

		// Check disjointness
		DisjointErrorEdge disjointEdge = isDisjoint(edgeQuery);
		if (disjointEdge != null)
			return disjointEdge;

		// Check collection order
		CollectionOrderErrorEdge collectionOrderEdge = isInvalidCollectionOrder(edgeQuery);
		if (collectionOrderEdge != null)
			return collectionOrderEdge;

		// Check symmetry
		CyclicErrorEdge cyclicEdge = isCyclic(edgeQuery, negated);
		if (cyclicEdge != null)
			return cyclicEdge;
		return null;
	}

	static {
		nodeFlags_.addFlag("allowVariables", false);

		edgeFlags_.addFlag("forceConstraints", false);
	}
}
