package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.EdgeModifier;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.inference.VariableNode;

import java.lang.instrument.IllegalClassFormatException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import util.UtilityMethods;
import util.collection.MultiMap;

public class DisjointModule extends DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 1L;

	public static final String DISJOINT_ID = "disj";

	/** Maps which node(s) are disjoint to the key node (at most general level) */
	private MultiMap<DAGNode, DAGNode> disjointMap_;

	private MultiMap<DAGNode, DAGNode> notDisjointMap_;

	private QueryModule queryModule_;

	public DisjointModule() {
		disjointMap_ = MultiMap.createConcurrentHashSetMultiMap();
		notDisjointMap_ = MultiMap.createConcurrentHashSetMultiMap();
	}

	/**
	 * Adds parent identifier(s) to a node, keeping them in sorted order.
	 * 
	 * @param n
	 *            The node to add to.
	 * @param parentID
	 *            The parent(s) to add to the node.
	 */
	private void addDisjointParents(DAGNode n, String nodeParentStr,
			String... parentID) {
		String[] writable = parentID;
		if (nodeParentStr != null) {
			if (nodeParentStr.equals(StringUtils.join(writable, ',')))
				return;
			ArrayList<String> parents = UtilityMethods
					.split(nodeParentStr, ',');
			for (String pID : parentID) {
				if (!parents.contains(pID))
					parents.add(pID);
			}
			Collections.sort(parents);
			writable = parents.toArray(new String[parents.size()]);
		}

		// Write the parents
		dag_.addProperty(n, DISJOINT_ID, StringUtils.join(writable, ','));
	}

	/**
	 * Gets all collections disjoint to the given input collections (i.e. the
	 * disjoint collections on the other side of the relationship). These are
	 * stored as the keys for a map, where the values are the corresponding
	 * disjoint edge for the key.
	 * 
	 * @param parentCollectionIDs
	 *            The collections to find disjoint collections for.
	 * @return All direct disjoint collections to the input collections.
	 */
	private Map<DAGNode, DAGNode> getDisjoints(
			Collection<DAGNode> parentCollectionIDs,
			MultiMap<DAGNode, DAGNode> disjMap) {
		Map<DAGNode, DAGNode> subMap = new HashMap<>();
		for (DAGNode n : parentCollectionIDs) {
			Collection<DAGNode> disjointCollection = disjMap.get(n);
			if (disjointCollection != null) {
				for (DAGNode n2 : disjointCollection)
					subMap.put(n2, n);
			}
		}
		return subMap;
	}

	/**
	 * Gets all parent disjoint nodes associated with this node, as defined by
	 * the parameter encoded into the node. By 'parent disjoint collection', I
	 * mean the collection explicitly given in the disjointness assertion.
	 * 
	 * @param node
	 *            The node to get parent disjoint collections for.
	 * @return The parent node(s) for the current node.
	 * @throws IllegalClassFormatException
	 */
	private Collection<DAGNode> getParents(DAGNode node)
			throws IllegalClassFormatException {
		String disjStr = node.getProperty(DISJOINT_ID);
		Collection<DAGNode> parents = new HashSet<>();

		// If ontology function
		if (node instanceof OntologyFunction) {
			Collection<DAGNode> genlNodes = queryModule_.functionResults(
					(OntologyFunction) node, CommonConcepts.RESULT_GENL);
			for (DAGNode genlNode : genlNodes)
				parents.addAll(getParents(genlNode));
		}

		if (disjStr == null)
			return parents;
		// TODO If not collection

		ArrayList<String> disjStrs = UtilityMethods.split(disjStr, ',');
		for (String str : disjStrs) {
			if (str.startsWith("-"))
				continue;
			Node parentNode = dag_.findOrCreateNode(str, null, false);
			if (parentNode instanceof DAGNode)
				parents.add((DAGNode) parentNode);
			else
				throw new IllegalClassFormatException(
						"Disjoint node should be a DAGNode!");
		}
		return parents;
	}

	/**
	 * Checks if a currently added disjoint edge is legal based on the
	 * definition of disjointness.
	 * 
	 * @param edge
	 *            The disjoint edge being added.
	 * @return True if there are no concepts that are a member of both instances
	 *         of the edge's collections.
	 */
	private boolean isLegalDisjointEdge(Node[] nodes) {
		Collection<Substitution> genlQuery = queryModule_
				.executeQuery(new QueryObject(true, false, CommonConcepts.AND
						.getNode(dag_), new OntologyFunction(
						CommonConcepts.GENLS.getNode(dag_),
						VariableNode.DEFAULT, nodes[1]), new OntologyFunction(
						CommonConcepts.GENLS.getNode(dag_),
						VariableNode.DEFAULT, nodes[2])));
		if (!genlQuery.isEmpty())
			return false;
		Collection<Substitution> isaQuery = queryModule_
				.executeQuery(new QueryObject(true, false, CommonConcepts.AND
						.getNode(dag_), new OntologyFunction(CommonConcepts.ISA
						.getNode(dag_), VariableNode.DEFAULT, nodes[1]),
						new OntologyFunction(CommonConcepts.ISA.getNode(dag_),
								VariableNode.DEFAULT, nodes[2])));
		if (!isaQuery.isEmpty())
			return false;
		return true;
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		// If the edge is a disjoint edge, record and propagate
		Node[] nodes = EdgeModifier.getUnmodNodes(edge, dag_);
		boolean negated = EdgeModifier.isNegated(edge, dag_);
		if (nodes[0].equals(CommonConcepts.DISJOINTWITH.getNode(dag_))) {
			// First check if the disjoint edge is valid
			if (!negated && !isLegalDisjointEdge(nodes))
				return false;

			// Add to map
			if (negated) {
				notDisjointMap_.put((DAGNode) nodes[1], (DAGNode) nodes[2]);
				notDisjointMap_.put((DAGNode) nodes[2], (DAGNode) nodes[1]);
			} else {
				disjointMap_.put((DAGNode) nodes[1], (DAGNode) nodes[2]);
				disjointMap_.put((DAGNode) nodes[2], (DAGNode) nodes[1]);
			}

			// Propagate down
			// Node 1
			Collection<Node> subCols = queryModule_.executeAndParseVar(
					new QueryObject(CommonConcepts.GENLS.getNode(dag_),
							VariableNode.DEFAULT, nodes[1]),
					VariableNode.DEFAULT.getName());
			subCols.add(nodes[1]);
			for (Node n : subCols) {
				if (n instanceof DAGNode) {
					addDisjointParents((DAGNode) n,
							((DAGNode) n).getProperty(DISJOINT_ID),
							nodes[1].getIdentifier());
				}
			}

			// Node 2
			subCols = queryModule_.executeAndParseVar(new QueryObject(
					CommonConcepts.GENLS.getNode(dag_), VariableNode.DEFAULT,
					nodes[2]), VariableNode.DEFAULT.getName());
			subCols.add(nodes[2]);
			for (Node n : subCols) {
				if (n instanceof DAGNode) {
					addDisjointParents((DAGNode) n,
							((DAGNode) n).getProperty(DISJOINT_ID),
							nodes[2].getIdentifier());
				}
			}
		}
		// If the edge is a genls, propagate disjoint relationships
		else if (nodes[0].equals(CommonConcepts.GENLS.getNode(dag_))
				&& !negated) {
			// Add any data from the parent
			if (nodes[1] instanceof DAGNode && nodes[2] instanceof DAGNode) {
				String parentStr = ((DAGNode) nodes[2])
						.getProperty(DISJOINT_ID);
				if (parentStr != null && !parentStr.isEmpty()) {
					String[] parentDisjs = UtilityMethods.splitToArray(
							parentStr, ',');
					Collection<Node> subCols = queryModule_.executeAndParseVar(
							new QueryObject(CommonConcepts.GENLS.getNode(dag_),
									VariableNode.DEFAULT, nodes[1]),
							VariableNode.DEFAULT.getName());
					subCols.add(nodes[1]);
					for (Node n : subCols) {
						if (n instanceof DAGNode)
							addDisjointParents((DAGNode) n,
									((DAGNode) n).getProperty(DISJOINT_ID),
									parentDisjs);
					}
				}
			}
		}

		return true;
	}

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		if (args.length == 1 && args[0] instanceof QueryObject)
			return execute(((QueryObject) args[0]).shouldJustify(), args);
		return execute(new QueryObject((Node[]) args));
	}

	public Collection<DAGNode> execute(QueryObject queryObj)
			throws IllegalArgumentException, ModuleException {
		try {
			// Get the precomputed parent collections for the first arg
			Collection<DAGNode> parentCollections = getParents(queryObj
					.getAtomic());
			if (parentCollections.isEmpty())
				return null;

			// Compare using disjoint map
			Collection<DAGNode> result = calculateDisjoint(queryObj,
					parentCollections, disjointMap_, false);
			if (result != null)
				return result;

			// Compare using non-disjoint map
			result = calculateDisjoint(queryObj, parentCollections,
					notDisjointMap_, true);
			return result;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	protected Collection<DAGNode> calculateDisjoint(QueryObject queryObj,
			Collection<DAGNode> parentCollections,
			MultiMap<DAGNode, DAGNode> disjointMap, boolean negated)
			throws IllegalClassFormatException {
		// Get the disjoint collections for the parents
		Map<DAGNode, DAGNode> disjMap = getDisjoints(parentCollections,
				disjointMap);
		Collection<DAGNode> disjointCollections = disjMap.keySet();

		if (!queryObj.isProof()) {
			// If variable, add and return results.
			for (DAGNode n : disjointCollections)
				queryObj.addResult(!negated,
						new Substitution(queryObj.getVariable(), n),
						(Node[]) null);
			return disjointCollections;
		} else {
			// If proof, check if true and add justification.
			int varIndex = (queryObj.getAtomicIndex() == 1) ? 2 : 1;
			Collection<DAGNode> otherCollections = getParents((DAGNode) queryObj
					.getNode(varIndex));
			disjointCollections.retainAll(otherCollections);
			if (!disjointCollections.isEmpty()) {
				if (queryObj.shouldJustify()) {
					// Build the justification
					buildJustification(queryObj, disjMap, disjointCollections,
							varIndex, negated);
				}
				queryObj.addResult(!negated, new Substitution(), (Node[]) null);
				return Collections.EMPTY_LIST;
			}
		}
		return null;
	}

	protected void buildJustification(QueryObject queryObj,
			Map<DAGNode, DAGNode> disjMap,
			Collection<DAGNode> disjointCollections, int varIndex,
			boolean negated) {
		DAGNode key = disjointCollections.iterator().next();
		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		List<Node[]> justification = queryObj.getJustification();

		// Add the first genls justification.
		QueryObject genlsA = new QueryObject(false, true, genls,
				queryObj.getAtomic(), disjMap.get(key));
		queryModule_.prove(genlsA);
		List<Node[]> justificationA = genlsA.getJustification();
		if (justificationA.size() != 1
				|| !Arrays.equals(justificationA.get(0), new Node[] { genls,
						disjMap.get(key), disjMap.get(key) }))
			justification.addAll(justificationA);

		// Add the disjoint edge.
		Node[] disjointEdge = new Node[] {
				CommonConcepts.DISJOINTWITH.getNode(dag_), disjMap.get(key),
				key };
		if (negated)
			disjointEdge = new Node[] { CommonConcepts.NOT.getNode(dag_),
					new OntologyFunction(disjointEdge) };
		if (dag_.findEdge(disjointEdge) == null) {
			disjointEdge = new Node[] {
					CommonConcepts.DISJOINTWITH.getNode(dag_), key,
					disjMap.get(key) };
			if (negated)
				disjointEdge = new Node[] { CommonConcepts.NOT.getNode(dag_),
						new OntologyFunction(disjointEdge) };
		}
		justification.add(disjointEdge);

		// Add the last genls justification (reversed).
		QueryObject genlsB = new QueryObject(false, true, genls,
				queryObj.getNode(varIndex), key);
		queryModule_.prove(genlsB);
		List<Node[]> justificationB = genlsB.getJustification();
		if (justificationB.size() != 1
				|| !Arrays.equals(justificationB.get(0), new Node[] { genls,
						key, key })) {
			Collections.reverse(justificationB);
			justification.addAll(justificationB);
		}
	}

	@Override
	public boolean initialisationComplete(Collection<DAGNode> nodes,
			Collection<DAGEdge> edges, boolean forceRebuild) {
		if (disjointMap_.isKeysEmpty()) {
			System.out.print("Building Disjoint Module map... ");
			defaultRebuild(nodes, false, edges, true);
			System.out.println("Done!");
			return true;
		}
		return false;
	}

	@Override
	public boolean removeEdge(DAGEdge edge) {
		System.err.println("Unsupported operation for DisjointModule!");
		Node[] nodes = EdgeModifier.getUnmodNodes(edge, dag_);
		boolean negated = EdgeModifier.isNegated(edge, dag_);
		if (nodes[0].equals(CommonConcepts.DISJOINTWITH.getNode(dag_))) {
			// Easy peasy, just remove the element from the map
			if (negated) {
				Collection<DAGNode> coll = notDisjointMap_.get(nodes[1]);
				if (coll != null)
					coll.remove(nodes[2]);
				coll = notDisjointMap_.get(nodes[2]);
				if (coll != null)
					coll.remove(nodes[1]);
			} else {
				Collection<DAGNode> coll = disjointMap_.get(nodes[1]);
				if (coll != null)
					coll.remove(nodes[2]);
				coll = disjointMap_.get(nodes[2]);
				if (coll != null)
					coll.remove(nodes[1]);
			}
		} else if (nodes[0].equals(CommonConcepts.GENLS.getNode(dag_))) {
			// TODO For each child of the arg1, recalculate disjoint
			// Queue<DAGNode> children = new LinkedList<>();
			// children.add((DAGNode) nodes[1]);
			// while (!children.isEmpty()) {
			//
			// }
		}
		return true;
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);
		queryModule_ = (QueryModule) dag_.getModule(QueryModule.class);
	}

	@Override
	public String toString() {
		return "DisjointWith Module: " + (disjointMap_.size() / 2)
				+ " assertions & " + (notDisjointMap_.size() / 2)
				+ " negated assertions";
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return !EdgeModifier.isRemoved(edge, dag_);
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}
}
