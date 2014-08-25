package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.VariableNode;

import java.lang.instrument.IllegalClassFormatException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;

import org.apache.commons.collections4.CollectionUtils;
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

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		// Return either all highest level collections that are disjoint with
		// the arg (if query) or an empty collection for true if a proof.
		if (args.length == 0)
			return null;

		try {
			// Get the precomputed parent collections for the first arg
			Collection<DAGNode> parentCollections = getParents((DAGNode) args[0]);

			// Get the disjoint collections for the parents
			Collection<DAGNode> disjointCollections = getDisjoints(parentCollections);
			int disjointSize = parentCollections.size();

			if (args.length == 1) {
				return disjointCollections;
			} else {
				Collection<DAGNode> otherCollections = getParents((DAGNode) args[1]);
				int otherSize = otherCollections.size();
				if (disjointSize < otherSize) {
					disjointCollections.retainAll(otherCollections);
					if (!disjointCollections.isEmpty())
						return disjointCollections;
				} else {
					otherCollections.retainAll(disjointCollections);
					if (!otherCollections.isEmpty())
						return otherCollections;
				}
				return null;
			}
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public boolean addEdge(DAGEdge edge) {
		// If the edge is a disjoint edge, record and propagate
		Node[] nodes = edge.getNodes();
		if (nodes[0].equals(CommonConcepts.DISJOINTWITH.getNode(dag_))) {
			// Add to map
			disjointMap_.put((DAGNode) nodes[1], (DAGNode) nodes[2]);
			disjointMap_.put((DAGNode) nodes[2], (DAGNode) nodes[1]);

			// Propagate down
			// Node 1
			Collection<Node> subCols = queryModule_.executeAndParseVar(
					new QueryObject(CommonConcepts.GENLS.getNode(dag_),
							VariableNode.DEFAULT, nodes[1]),
					VariableNode.DEFAULT.getName());
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
					String nodeParentStr = ((DAGNode) nodes[1])
							.getProperty(DISJOINT_ID);
					if (!parentStr.equals(nodeParentStr))
						addDisjointParents((DAGNode) nodes[1], nodeParentStr,
								UtilityMethods.splitToArray(parentStr, ','));
				}
			}
		}

		return true;
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

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);
		queryModule_ = (QueryModule) dag_.getModule(QueryModule.class);
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
		// TODO Auto-generated method stub
		return super.removeEdge(edge);
	}

	/**
	 * Gets all collections disjoint to the given input collections (i.e. the
	 * disjoint collections on the other side of the relationship).
	 * 
	 * @param parentCollectionIDs
	 *            The collections to find disjoint collections for.
	 * @return All direct disjoint collections to the input collections.
	 */
	private Collection<DAGNode> getDisjoints(
			Collection<DAGNode> parentCollectionIDs) {
		Collection<DAGNode> disjointCollections = new HashSet<>();
		for (DAGNode n : parentCollectionIDs) {
			Collection<DAGNode> disjointCollection = disjointMap_.get(n);
			if (disjointCollection != null)
				disjointCollections.addAll(disjointCollection);
		}
		return disjointCollections;
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

}
