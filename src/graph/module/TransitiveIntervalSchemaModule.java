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
package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class TransitiveIntervalSchemaModule extends
		DAGModule<Collection<DAGNode>> {
	private static final float INTERVAL_SPLIT = 0.5f;
	private static final long serialVersionUID = 6562719667555853873L;
	private static final String TEMP_MARK = "tmp";
	public static final String ANCESTOR_ID = "ancsID";
	public static final int INITIAL_INTERVAL = 16;
	public static final String MARK = "topMark";
	public static final String PERMANENT_MARK = "prm";
	public static final String PREDECESSOR_ID = "predID";
	private IntervalSchema ancestorMap_;
	private int interval_ = INITIAL_INTERVAL;
	private IntervalSchema predecessorMap_;
	private transient QueryModule queryModule_;
	private transient RelatedEdgeModule relEdgeModule_;
	private DAGNode virtualRoot_;
	protected DAGNode transitiveNode_;
	private boolean incrementalSupported_ = false;

	/**
	 * Builds a spanning tree for a given topologically sorted collection of
	 * nodes.
	 * 
	 * @param topologicalList
	 *            The topologically sorted list of nodes.
	 * @param predecessorTree
	 *            If this tree is building a predecessor tree (alt. ancestor
	 *            tree).
	 * @return The spanning tree for the nodes.
	 */
	private Map<DAGNode, TreeNode> buildSpanningTree(
			List<DAGNode> topologicalList, boolean predecessorTree) {
		TreeNode virtualRootNode = new TreeNode();
		Map<DAGNode, TreeNode> spanningTree = new HashMap<>();
		spanningTree.put(virtualRoot_, virtualRootNode);

		// Iterate through the list (backwards if ancestors)
		ListIterator<DAGNode> iter = null;
		if (predecessorTree)
			iter = topologicalList.listIterator();
		else
			iter = topologicalList.listIterator(topologicalList.size());
		while ((predecessorTree && iter.hasNext())
				|| (!predecessorTree && iter.hasPrevious())) {
			DAGNode n = (predecessorTree) ? iter.next() : iter.previous();

			dag_.removeProperty(n, MARK);
			// For every incoming edge

			Collection<DAGNode> transitiveNodes = getTransitiveNodes(n,
					!predecessorTree);

			DAGNode currentNode = null;
			int currentSize = -1;
			for (DAGNode edgeNode : transitiveNodes) {
				if (edgeNode.equals(n))
					continue;
				if (!spanningTree.containsKey(edgeNode))
					continue;
				if (currentNode == null) {
					currentNode = edgeNode;
					currentSize = spanningTree.get(currentNode).trans_.size();
				} else {
					int edgeSize = spanningTree.get(edgeNode).trans_.size();
					if (currentSize < edgeSize) {
						currentNode = edgeNode;
						currentSize = edgeSize;
					}
				}
			}

			// Set predecessors as remaining incoming edges + their predecessors
			if (currentNode == null) {
				spanningTree.put(n, new TreeNode());
				virtualRootNode.children_.add(n);
			} else {
				// Predecessor info
				TreeNode thisTreeNode = new TreeNode();
				thisTreeNode.trans_.add(currentNode);
				TreeNode otherTreeNode = spanningTree.get(currentNode);
				thisTreeNode.trans_.addAll(otherTreeNode.trans_);
				spanningTree.put(n, thisTreeNode);

				// Ancestor info
				otherTreeNode.children_.add(n);
			}
		}
		return spanningTree;
	}

	private Collection<DAGNode> getTransitiveNodes(DAGNode node,
			boolean isUpwards) {
		int nIndex = (isUpwards) ? 2 : 3;
		Collection<Edge> edges = relEdgeModule_.execute(transitiveNode_, 1,
				node, nIndex);
		Collection<DAGNode> transitiveNodes = new HashSet<>(edges.size());
		int tIndex = (isUpwards) ? 2 : 1;
		for (Edge edge : edges)
			transitiveNodes.add((DAGNode) edge.getNodes()[tIndex]);

		// Function chasing
		if (transitiveNode_.equals(CommonConcepts.GENLS.getNode(dag_))
				&& isUpwards && node instanceof OntologyFunction)
			transitiveNodes.addAll(queryModule_.functionResults(
					(OntologyFunction) node, CommonConcepts.RESULT_GENL));
		return transitiveNodes;
	}

	private boolean rebuildTrees() {
		ancestorMap_ = null;
		predecessorMap_ = null;
		initialisationComplete(dag_.getNodes(), dag_.getEdges(), false);
		return true;
	}

	@Override
	public boolean addEdge(Edge edge) {
		if (!isReady())
			return true;

		if (!incrementalSupported_) {
			System.err.println("Incremental updates not supported "
					+ "for TransitiveIntervalSchemaModule!");
			return true;
		}

		if (edge.getNodes()[0].equals(transitiveNode_)) {
			DAGNode nodeObj = (DAGNode) edge.getNodes()[1];
			String objIntIDStr = nodeObj.getProperty(PREDECESSOR_ID);
			DAGNode nodeSubj = (DAGNode) edge.getNodes()[2];
			String subjIntIDStr = nodeSubj.getProperty(PREDECESSOR_ID);
			if (objIntIDStr == null) {
				// genls NEW exist

				// Create new root node for predecessorTree
				if (!predecessorMap_.addNewRootedNode(nodeObj))
					return rebuildTrees();

				// Attach to parent descendant node in ancestorTree
				if (!ancestorMap_.addNewLeafNode(nodeObj,
						Integer.parseInt(nodeSubj.getProperty(ANCESTOR_ID))))
					return rebuildTrees();
			}
			if (subjIntIDStr == null) {
				// genls exist NEW

				// Create new root node for ancestorTree
				if (!ancestorMap_.addNewRootedNode(nodeSubj))
					return rebuildTrees();
				// Attach to parent descendant node in predecessorTree
				if (!predecessorMap_.addNewLeafNode(nodeSubj,
						Integer.parseInt(objIntIDStr)))
					return rebuildTrees();
			}

			// Propagate the intervals
			predecessorMap_.propagateIntervalAddition(nodeObj, nodeSubj,
					ancestorMap_, false);
			ancestorMap_.propagateIntervalAddition(nodeSubj, nodeObj,
					predecessorMap_, false);
		}
		return true;
	}

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		if (args.length < 2)
			return null;

		boolean upwards = (boolean) args[0];
		IntervalSchema schema = (upwards) ? predecessorMap_ : ancestorMap_;
		DAGNode node = (DAGNode) args[1];
		if (args.length >= 3) {
			DAGNode otherNode = (DAGNode) args[2];
			if (schema.isTransitive(node, otherNode))
				return new ArrayList<DAGNode>(0);
			else
				return null;
		}
		return schema.getTransitivePredecessors(node, false);
	}

	public DAGNode getTransitiveNode() {
		return transitiveNode_;
	}

	@Override
	public boolean initialisationComplete(Collection<DAGNode> nodes,
			Collection<DAGEdge> edges, boolean forceRebuild) {
		if (isReady() && !forceRebuild)
			return false;
		virtualRoot_ = null;
		initMembers();
		System.out.print("Creating transitive interval schema for "
				+ transitiveNode_ + "... ");

		// Sort topologically
		List<DAGNode> topologicalList = topologicalList(nodes);

		// Build spanning tree
		Map<DAGNode, TreeNode> predecessors = buildSpanningTree(
				topologicalList, true);
		Map<DAGNode, TreeNode> ancestors = buildSpanningTree(topologicalList,
				false);

		// Assign numbers and intervals
		IntervalSchema predecessorMap = new IntervalSchema(predecessors.size(),
				PREDECESSOR_ID);
		predecessorMap.assignIntervals(predecessors);
		IntervalSchema ancestorMap = new IntervalSchema(ancestors.size(),
				ANCESTOR_ID);
		ancestorMap.assignIntervals(ancestors);

		// Link all nodes
		predecessorMap.linkIntervals(topologicalList, true);
		ancestorMap.linkIntervals(topologicalList, false);
		predecessorMap_ = predecessorMap;
		ancestorMap_ = ancestorMap;
		System.out.println("Done!");
		return true;
	}

	/**
	 * Checks if the members are initialised.
	 */
	public void initMembers() {
		if (transitiveNode_ == null)
			transitiveNode_ = CommonConcepts.GENLS.getNode(dag_);
		if (relEdgeModule_ == null)
			relEdgeModule_ = (RelatedEdgeModule) dag_
					.getModule(RelatedEdgeModule.class);
		if (queryModule_ == null)
			queryModule_ = (QueryModule) dag_.getModule(QueryModule.class);
		if (virtualRoot_ == null)
			virtualRoot_ = new DAGNode();
	}

	public boolean isReady() {
		return predecessorMap_ != null && ancestorMap_ != null;
	}

	@Override
	public void disableCached() {
		predecessorMap_ = null;
		ancestorMap_ = null;
	}

	public List<Node[]> justifyTransitive(DAGNode baseNode, DAGNode transNode) {
		List<Node[]> justification = new ArrayList<Node[]>();
		justifyTransitiveR(baseNode, transNode, justification,
				new HashSet<DAGNode>());
		Collections.reverse(justification);

		return justification;
	}

	public List<Node[]> justifyTransitiveR(DAGNode baseNode, DAGNode transNode,
			List<Node[]> justification, Set<DAGNode> seenNodes) {
		if (baseNode.equals(transNode))
			return justification;

		for (DAGNode directNext : getTransitiveNodes(baseNode, true)) {
			if (!seenNodes.contains(directNext)
					&& predecessorMap_.isTransitive(directNext, transNode)) {
				seenNodes.add(directNext);
				if (justifyTransitiveR(directNext, transNode, justification,
						seenNodes) != null) {
					justification.add(new Node[] { transitiveNode_, baseNode,
							directNext });
					return justification;
				}
			}
		}
		return null;
	}

	@Override
	public boolean removeEdge(Edge edge) {
		if (!isReady())
			return true;

		if (!incrementalSupported_) {
			System.err.println("Incremental updates not supported "
					+ "for TransitiveIntervalSchemaModule!");
			return true;
		}

		if (edge.getNodes()[0].equals(transitiveNode_)) {
			DAGNode nodeObj = (DAGNode) edge.getNodes()[1];
			DAGNode nodeSubj = (DAGNode) edge.getNodes()[2];
			predecessorMap_.removeEdge(nodeObj, nodeSubj, ancestorMap_);
			ancestorMap_.removeEdge(nodeSubj, nodeObj, predecessorMap_);
		}
		return true;
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);
		initMembers();
	}

	@Override
	public Collection<String> getPertinentProperties() {
		Collection<String> props = new ArrayList<String>(2);
		props.add(ANCESTOR_ID);
		props.add(PREDECESSOR_ID);
		return props;
	}

	/**
	 * Topologically sorts a collection of nodes.
	 * 
	 * @param nodes
	 *            The nodes to sort.
	 * @return The topologically sorted list of nodes.
	 */
	public List<DAGNode> topologicalList(Collection<DAGNode> nodes) {
		LinkedList<DAGNode> sortedNodes = new LinkedList<>();
		for (DAGNode n : nodes) {
			topologicalVisit(n, sortedNodes, new HashMap<DAGNode, String>());
		}
		return sortedNodes;
	}

	/**
	 * Visits a node and follows it up to establish a topological sort.
	 * 
	 * @param n
	 *            The node to check.
	 * @param sortedNodes
	 *            The currently topologically sorted nodes
	 * @param markMap
	 *            A map to mark scanned nodes.
	 * @return A node if there is a cycle, otherwise null.
	 */
	public DAGNode topologicalVisit(DAGNode n, LinkedList<DAGNode> sortedNodes,
			HashMap<DAGNode, String> markMap) {
		String permaMark = n.getProperty(MARK);
		if (permaMark != null && permaMark.equals(PERMANENT_MARK))
			return null;

		String mark = markMap.get(n);
		if (mark != null && mark.equals(TEMP_MARK)) {
			System.out.print("Cycle");
			// Does it have a rewrite of?
			if (!relEdgeModule_.execute(
					CommonConcepts.REWRITE_OF.getNode(dag_), "1", n, "-F")
					.isEmpty())
				System.out.print(" (has rewriteOf)");
			System.out.print(": " + n);
			return n;
		}
		DAGNode cycleNode = null;
		if (mark == null) {
			markMap.put(n, TEMP_MARK);
			Collection<DAGNode> transitiveNodes = getTransitiveNodes(n, true);
			for (DAGNode edgeNode : transitiveNodes) {
				if (!edgeNode.equals(n)) {
					DAGNode tempNode = topologicalVisit(edgeNode, sortedNodes,
							markMap);
					if (cycleNode == null)
						cycleNode = tempNode;
					if (tempNode != null) {
						System.out.print(" -> " + n);
						if (n.equals(tempNode)) {
							System.out.println();
							cycleNode = null;
						}
					}
				}
			}
			dag_.addProperty(n, MARK, PERMANENT_MARK);
			markMap.put(n, PERMANENT_MARK);
			sortedNodes.push(n);
		}
		return cycleNode;
	}

	private class IDOrder implements Comparator<DAGNode> {
		private String intervalIDKey_;
		private int order_;

		public IDOrder(String intervalIDKey, boolean isReversed) {
			intervalIDKey_ = intervalIDKey;
			order_ = (isReversed) ? -1 : 1;
		}

		@Override
		public int compare(DAGNode arg0, DAGNode arg1) {

			return order_
					* Integer.compare(
							Integer.parseInt(arg0.getProperty(intervalIDKey_)),
							Integer.parseInt(arg1.getProperty(intervalIDKey_)));
		}

	}

	/**
	 * The interval schema that forms the lookup data structure.
	 * 
	 * @author Sam Sarjant
	 */
	private class IntervalSchema implements Serializable {
		private static final long serialVersionUID = -5807751667326507399L;
		private String intervalIDKey_;
		private SortedMap<Integer, DAGNode> inverseMap_;
		private int maxIndex_;
		private Map<Integer, Collection<int[]>> schemaMap_;
		private transient Lock schemaLock_;

		public IntervalSchema(int size, String intervalIDKey) {
			schemaMap_ = new ConcurrentHashMap<>(size);
			inverseMap_ = new TreeMap<>();
			intervalIDKey_ = intervalIDKey;
			schemaLock_ = new ReentrantLock();
		}

		/**
		 * Adds all intervals associated with n2 to n1, subsuming where
		 * appropriate.
		 * 
		 * @param n1
		 *            The node to add intervals to.
		 * @param n2
		 *            The node to add intervals from.
		 * @return True if the intervals were changed as a result of this call.
		 */
		private boolean addIntervals(DAGNode n1, DAGNode n2) {
			if (n1.getProperty(intervalIDKey_) == null)
				return false;
			Integer n1ID = Integer.parseInt(n1.getProperty(intervalIDKey_));
			if (n2.getProperty(intervalIDKey_) == null)
				return false;
			Integer n2ID = Integer.parseInt(n2.getProperty(intervalIDKey_));
			Collection<int[]> n1Intervals = schemaMap_.get(n1ID);
			Collection<int[]> n2Intervals = schemaMap_.get(n2ID);

			boolean changed = false;
			for (int[] intervalN2 : n2Intervals) {
				boolean addNew = true;
				// TODO Merge intervals when appropriate
				// Check for subsumption
				for (int[] intervalN1 : n1Intervals) {
					if (intervalN1[0] <= intervalN2[0]
							&& intervalN1[1] >= intervalN2[1]) {
						// 1_0, 2_0, 2_1, 1_1
						addNew = false;
					} else {
						if (intervalN2[0] < intervalN1[0]
								&& intervalN2[1] >= intervalN1[0]) {
							// 2_0, 1_0, 2_1
							intervalN1[0] = intervalN2[0];
							addNew = false;
							changed = true;
						}
						if (intervalN2[1] > intervalN1[1]
								&& intervalN2[0] <= intervalN1[1]) {
							// 2_0, 1_1, 2_1
							intervalN1[1] = intervalN2[1];
							addNew = false;
							changed = true;
						}
					}

					if (!addNew)
						break;
				}

				if (addNew)
					n1Intervals.add(Arrays.copyOf(intervalN2, 2));
				changed |= addNew;
			}
			return changed;
		}

		/**
		 * Traverse a spanning tree in post-order (LRC) fashion. Assign IDs to
		 * the nodes and record the intervals.
		 * 
		 * @param node
		 *            The current node.
		 * @param currentIndex
		 *            The current ID to assign.
		 * @param internalIDKey
		 *            The key under which the internals IDs are recorded on the
		 *            node.
		 * @param spanningTree
		 *            The spanning tree.
		 * @return The next ID after this one.
		 */
		private int postOrderTraverse(DAGNode node, int currentIndex,
				String internalIDKey, Map<DAGNode, TreeNode> spanningTree) {
			// Children first
			int prevIndex = currentIndex;
			TreeNode spanNode = spanningTree.get(node);
			for (DAGNode child : spanNode.children_) {
				currentIndex = postOrderTraverse(child, currentIndex,
						internalIDKey, spanningTree);
			}

			// Assign ID and interval
			currentIndex += interval_;
			dag_.addProperty(node, internalIDKey, currentIndex + "");
			recordID(node, currentIndex, new int[] { prevIndex + 1,
					currentIndex });

			return currentIndex;
		}

		/**
		 * Records ID details for a given node.
		 * 
		 * @param node
		 *            The node to record details for.
		 * @param id
		 *            The spanning tree ID of the node.
		 * @param interval
		 *            The interval of transitive values to record for the node.
		 */
		private void recordID(DAGNode node, int id, int[] interval) {
			if (schemaLock_ == null)
				schemaLock_ = new ReentrantLock();
			schemaLock_.lock();
			inverseMap_.put(id, node);
			Collection<int[]> intervals = new ArrayList<>(1);
			intervals.add(interval);
			schemaMap_.put(id, intervals);
			schemaLock_.unlock();
		}

		/**
		 * Removes all intervals associated with n2 from n1, reducing interval
		 * scope where appropriate.
		 * 
		 * @param n1
		 *            The node to remove intervals from.
		 * @param n2
		 *            This node's intervals are removed from the other node.
		 * @return True if intervals were changed as a result of this call.
		 */
		private boolean removeIntervals(DAGNode n1, DAGNode n2) {
			if (n1.equals(n2))
				return false;
			Integer n1ID = Integer.parseInt(n1.getProperty(intervalIDKey_));
			Collection<int[]> n1Intervals = schemaMap_.get(n1ID);
			Integer n2ID = Integer.parseInt(n2.getProperty(intervalIDKey_));
			boolean changed = false;
			for (int[] intervalN2 : schemaMap_.get(n2ID)) {
				int[] splitInterval = null;
				// Check for subsumption
				for (Iterator<int[]> iter = n1Intervals.iterator(); iter
						.hasNext();) {
					int[] intervalN1 = iter.next();
					if (intervalN1[0] < intervalN2[0]
							&& intervalN1[1] > intervalN2[1]) {
						// 1_0, 2_0, 2_1, 1_1
						splitInterval = new int[] { intervalN2[1] + 1,
								intervalN1[1] };
						intervalN1[1] = intervalN2[0] - 1;
					} else {
						if (intervalN2[0] <= intervalN1[0]
								&& intervalN2[1] > intervalN1[0]) {
							// 2_0, 1_0, 2_1
							intervalN1[0] = intervalN2[1] + 1;
							changed = true;
						}
						if (intervalN2[1] >= intervalN1[1]
								&& intervalN2[0] < intervalN1[1]) {
							// 2_0, 1_1, 2_1
							intervalN1[1] = intervalN2[0] - 1;
							changed = true;
						}
						if (intervalN1[0] > intervalN1[1])
							iter.remove();
					}
				}

				if (splitInterval != null) {
					n1Intervals.add(splitInterval);
					changed = true;
				}
			}
			return changed;
		}

		/**
		 * Adds a new node as a child of another node.
		 * 
		 * @param nodeSubj
		 *            The node to add.
		 * @param objKey
		 *            The key of the parent to add to.
		 * @return True if the node was added. False if the tree needs to be
		 *         rebuilt.
		 */
		public synchronized boolean addNewLeafNode(DAGNode nodeSubj, int objKey) {
			SortedMap<Integer, DAGNode> headMap = inverseMap_.headMap(objKey);
			int prevIndex = (headMap != null && !headMap.isEmpty()) ? headMap
					.lastKey() : 0;
			int childID = (int) Math.ceil(prevIndex * INTERVAL_SPLIT + objKey
					* (1 - INTERVAL_SPLIT));
			if (inverseMap_.containsKey(childID))
				return false;

			dag_.addProperty(nodeSubj, intervalIDKey_, childID + "");
			recordID(nodeSubj, childID, new int[] { prevIndex + 1, childID });
			return true;
		}

		/**
		 * Adds a new node to the virtual root of the tree.
		 * 
		 * @param node
		 *            The node being added.
		 * @return True if the node was added. False if the tree needs to be
		 *         rebuilt.
		 */
		public synchronized boolean addNewRootedNode(DAGNode node) {
			int prevIndex = maxIndex_;
			if (maxIndex_ > Integer.MAX_VALUE - interval_) {
				// Too many indices. Reduce interval and restructure tree
				interval_ /= 2;
				return false;
			}

			maxIndex_ += interval_;
			dag_.addProperty(node, intervalIDKey_, maxIndex_ + "");
			recordID(node, maxIndex_, new int[] { prevIndex + 1, maxIndex_ });
			return true;
		}

		/**
		 * Assigns initial IDs and intervals to the set of nodes in a spanning
		 * tree.
		 * 
		 * @param spanningTree
		 *            The tree of nodes to assign IDs to.
		 * @return The resulting IntervalSchema.
		 */
		public IntervalSchema assignIntervals(
				Map<DAGNode, TreeNode> spanningTree) {
			IntervalSchema intervalSchema = new IntervalSchema(
					spanningTree.size() * 2, intervalIDKey_);
			maxIndex_ = postOrderTraverse(virtualRoot_, 0, intervalIDKey_,
					spanningTree);
			recordID(virtualRoot_, maxIndex_,
					new int[] { 0, Integer.MAX_VALUE });
			return intervalSchema;
		}

		/**
		 * Gets all transitive predecessors for a given node, ordered by their
		 * post-order IDs.
		 * 
		 * @param node
		 *            The node to get the transitive predecessors for.
		 * @param reversed
		 *            If the order of elements should be reversed.
		 * @return All transitively accessed nodes.
		 */
		public Collection<DAGNode> getTransitivePredecessors(DAGNode node,
				boolean reversed) {
			String predIDStr = node.getProperty(intervalIDKey_);
			if (predIDStr == null)
				return null;
			int predID = Integer.parseInt(predIDStr);
			Collection<int[]> intervals = schemaMap_.get(predID);
			Collection<DAGNode> transitive = new TreeSet<>(new IDOrder(
					intervalIDKey_, reversed));
			for (int[] interval : intervals)
				transitive.addAll(inverseMap_.subMap(interval[0],
						interval[1] + 1).values());

			return transitive;
		}

		/**
		 * Asks if a given node is transitively higher than another.
		 * 
		 * @param node
		 *            The base node.
		 * @param otherNode
		 *            The query node.
		 * @return True if the otherNode is transitively accessed from node.
		 */
		public boolean isTransitive(DAGNode node, DAGNode otherNode) {
			String predIDStr = node.getProperty(intervalIDKey_);
			if (predIDStr == null)
				return false;
			int predID = Integer.parseInt(predIDStr);

			String otherIDStr = otherNode.getProperty(intervalIDKey_);
			if (otherIDStr == null)
				return false;
			int otherID = Integer.parseInt(otherIDStr);

			Collection<int[]> intervals = schemaMap_.get(predID);
			for (int[] interval : intervals)
				if (otherID >= interval[0] && otherID <= interval[1])
					return true;
			return false;
		}

		/**
		 * Links the nodes with non-spanning tree intervals.
		 * 
		 * @param topologicalList
		 *            The nodes sorted topologically.
		 * @param schema
		 *            The interval schema to add to.
		 * @param predecessorTree
		 *            If the interval schema is for predecessors (else
		 *            ancestors).
		 */
		public void linkIntervals(List<DAGNode> topologicalList,
				boolean predecessorTree) {
			ListIterator<DAGNode> iter = null;
			if (predecessorTree)
				iter = topologicalList.listIterator(topologicalList.size());
			else
				iter = topologicalList.listIterator();
			while ((predecessorTree && iter.hasPrevious())
					|| (!predecessorTree && iter.hasNext())) {
				DAGNode n = (predecessorTree) ? iter.previous() : iter.next();
				Collection<DAGNode> transitiveNodes = getTransitiveNodes(n,
						predecessorTree);
				for (DAGNode edgeNode : transitiveNodes) {
					if (edgeNode.equals(n) || edgeNode.isAnonymous())
						continue;
					addIntervals(n, edgeNode);
				}
			}
		}

		/**
		 * Adds all intervals associated with interval node to base node and
		 * propagates the changes to predecessors as much as necessary.
		 * 
		 * @param baseNode
		 *            The node to add the intervals to.
		 * @param intervalNode
		 *            The node from which the intervals are added.
		 */
		public void propagateIntervalAddition(DAGNode baseNode,
				DAGNode intervalNode, IntervalSchema mirrorSchema,
				boolean removeFirst) {
			Collection<DAGNode> predecessors = mirrorSchema
					.getTransitivePredecessors(baseNode, true);
			for (DAGNode predecessor : predecessors) {
				// TODO Can stop early if ordered correctly.
				addIntervals(predecessor, intervalNode);
			}
		}

		/**
		 * Removes an edge from the tree.
		 * 
		 * @param nodeObj
		 *            The object of the edge.
		 * @param nodeSubj
		 *            The subject of the edge.
		 * @param mirrorSchema
		 */
		public void removeEdge(DAGNode nodeObj, DAGNode nodeSubj,
				IntervalSchema mirrorSchema) {
			// Propagate the intervals
			Collection<DAGNode> predecessors = mirrorSchema
					.getTransitivePredecessors(nodeObj, true);

			if (schemaLock_ == null)
				schemaLock_ = new ReentrantLock();
			schemaLock_.lock();
			for (DAGNode predecessor : predecessors) {
				// Remove the subjIntervals from the obj
				removeIntervals(predecessor, nodeSubj);
				Collection<DAGNode> transitives = getTransitiveNodes(
						predecessor, intervalIDKey_.equals(PREDECESSOR_ID));
				for (DAGNode n : transitives)
					addIntervals(predecessor, n);
			}
			schemaLock_.unlock();
		}
	}

	private class TreeNode {
		Collection<DAGNode> children_;
		Collection<DAGNode> trans_;

		public TreeNode() {
			trans_ = new ArrayList<>();
			children_ = new ArrayList<>();
		}
	}
}
