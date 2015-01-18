package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.inference.VariableNode;

import java.lang.instrument.IllegalClassFormatException;
import java.util.ArrayList;
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

	private QueryModule queryModule_;

	public DisjointModule() {
		disjointMap_ = MultiMap.createConcurrentHashSetMultiMap();
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
			Collection<DAGNode> parentCollectionIDs) {
		Map<DAGNode, DAGNode> disjMap = new HashMap<>();
		for (DAGNode n : parentCollectionIDs) {
			Collection<DAGNode> disjointCollection = disjointMap_.get(n);
			if (disjointCollection != null) {
				for (DAGNode n2 : disjointCollection)
					disjMap.put(n2, n);
			}
		}
		return disjMap;
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
	private boolean isLegalDisjointEdge(DAGEdge edge) {
		Node[] nodes = edge.getNodes();
		Collection<Substitution> genlQuery = queryModule_.execute(
				CommonConcepts.AND.getNode(dag_), new OntologyFunction(
						CommonConcepts.GENLS.getNode(dag_),
						VariableNode.DEFAULT, nodes[1]), new OntologyFunction(
						CommonConcepts.GENLS.getNode(dag_),
						VariableNode.DEFAULT, nodes[2]));
		if (!genlQuery.isEmpty())
			return false;
		Collection<Substitution> isaQuery = queryModule_.execute(
				CommonConcepts.AND.getNode(dag_), new OntologyFunction(
						CommonConcepts.ISA.getNode(dag_), VariableNode.DEFAULT,
						nodes[1]),
				new OntologyFunction(CommonConcepts.ISA.getNode(dag_),
						VariableNode.DEFAULT, nodes[2]));
		if (!isaQuery.isEmpty())
			return false;
		return true;
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		// If the edge is a disjoint edge, record and propagate
		Node[] nodes = edge.getNodes();
		if (nodes[0].equals(CommonConcepts.DISJOINTWITH.getNode(dag_))) {
			// First check if the disjoint edge is valid
			if (!isLegalDisjointEdge(edge))
				return false;

			// Add to map
			disjointMap_.put((DAGNode) nodes[1], (DAGNode) nodes[2]);
			disjointMap_.put((DAGNode) nodes[2], (DAGNode) nodes[1]);

			// Propagate down
			// Node 1
			Collection<Node> subCols = queryModule_.executeAndParseVar(
					new QueryObject(CommonConcepts.GENLS.getNode(dag_),
							VariableNode.DEFAULT, nodes[1]),
					VariableNode.DEFAULT.getName());
			subCols.add(nodes[1]);
			for (Node n : subCols) {
				if (n instanceof DAGNode)
					addDisjointParents((DAGNode) n,
							((DAGNode) n).getProperty(DISJOINT_ID),
							nodes[1].getIdentifier());
			}

			// Node 2
			subCols = queryModule_.executeAndParseVar(new QueryObject(
					CommonConcepts.GENLS.getNode(dag_), VariableNode.DEFAULT,
					nodes[2]), VariableNode.DEFAULT.getName());
			subCols.add(nodes[2]);
			for (Node n : subCols) {
				if (n instanceof DAGNode)
					addDisjointParents((DAGNode) n,
							((DAGNode) n).getProperty(DISJOINT_ID),
							nodes[2].getIdentifier());
			}
		}
		// If the edge is a genls, propagate disjoint relationships
		if (nodes[0].equals(CommonConcepts.GENLS.getNode(dag_))) {
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

	@SuppressWarnings("unchecked")
	public Collection<DAGNode> execute(QueryObject queryObj)
			throws IllegalArgumentException, ModuleException {
		try {
			// Get the precomputed parent collections for the first arg
			Collection<DAGNode> parentCollections = getParents(queryObj
					.getAtomic());
			if (parentCollections.isEmpty())
				return null;

			// Get the disjoint collections for the parents
			Map<DAGNode, DAGNode> disjMap = getDisjoints(parentCollections);
			Collection<DAGNode> disjointCollections = disjMap.keySet();

			if (!queryObj.isProof()) {
				// If variable, add and return results.
				for (DAGNode n : disjointCollections)
					queryObj.addResult(new Substitution(queryObj.getVariable(),
							n), (Node[]) null);
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
						DAGNode key = disjointCollections.iterator().next();
						DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
						List<Node[]> justification = queryObj
								.getJustification();
						justification.addAll(queryModule_.justify(genls,
								queryObj.getAtomic(), disjMap.get(key)));
						justification.add(new Node[] {
								CommonConcepts.DISJOINTWITH.getNode(dag_),
								disjMap.get(key), key });
						List<Node[]> revJust = queryModule_.justify(genls,
								queryObj.getNode(varIndex), key);
						Collections.reverse(revJust);
						justification.addAll(revJust);
					}
					queryObj.addResult(new Substitution(), (Node[]) null);
					return Collections.EMPTY_LIST;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
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
				+ " assertions";
	}
}
