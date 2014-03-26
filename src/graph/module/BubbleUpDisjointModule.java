/*******************************************************************************
 * Copyright (c) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Yichen Hu - initial API and implementation
 ******************************************************************************/
package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.StringNode;
import graph.inference.CommonQuery;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

import util.Pair;

/**
 * The objective of this module is to merge low level disjoint edges to a higher
 * level disjoint edge as many as possible.
 * 
 * The module will explore existing disjoint edges with a button-up sequence.
 * Each time a disjoint edge found, for example, between node A and node B, the
 * method will see if it's possible to create a disjoint from each parent node
 * of node A to node B.
 */
public class BubbleUpDisjointModule extends DAGModule<Collection<DAGEdge>> {
	private static final long serialVersionUID = -5748776310515109216L;
	private transient RelatedEdgeModule relEdgeModule_;
	private transient TransitiveIntervalSchemaModule transitiveModule_;
	private transient QueryModule queryModule_;
	/*
	 * Assuming the maximum distance from "Thing"(depth) for every disjoint edge
	 * is less than 100
	 */
	private static int ARRAYSIZE = 100;
	// ArrayList of ArrayLists, with fixed size 100(depth<=100)
	private List<List<Pair<Node, Node>>> toExplore_ = new ArrayList<List<Pair<Node, Node>>>(
			Collections.nCopies(100, new ArrayList<Pair<Node, Node>>()));
	private HashSet<Pair<Node, Node>> exploredPairs_ = new HashSet<Pair<Node, Node>>();

	/*
	 * Node: toExplore_ and exploredPairs_ represent different set of pairs,
	 * their base sets are always disjointed.
	 * 
	 * toExplore_'s pairs come directly from each node of each d-edge, like
	 * n1->n2
	 * 
	 * exploredPairs_'s pairs come from one node of an d-edge + one parent of
	 * the other node of the d-edge, like n1->n2.p1, n1->n2.p2 etc.
	 */

	// Constructor, not used yet
	public BubbleUpDisjointModule() {
	}

	@Override
	public Collection<DAGEdge> execute(Object... arg0)
			throws IllegalArgumentException, ModuleException {
		// TODO: manually call the bubble up process
		
		initialisationComplete(dag_.getNodes(), dag_.getEdges(), false);
		return null;
	}

	@Override
	public boolean initialisationComplete(Collection<DAGNode> nodes,
			Collection<DAGEdge> edges, boolean forceRebuild) {
		System.out.print("Starting to bubble up disjoints...");
		Node creator = new StringNode("BubbleUpDisjointModule");

		relEdgeModule_ = (RelatedEdgeModule) dag_
				.getModule(RelatedEdgeModule.class);
		transitiveModule_ = (TransitiveIntervalSchemaModule) dag_
				.getModule(TransitiveIntervalSchemaModule.class);
		queryModule_ = (QueryModule) dag_.getModule(QueryModule.class);

		// TODO: Check depth module and TransitiveIntervalSchema module have
		// done the work

		// Get all disjoint edges
		Collection<Edge> disjointEdges = relEdgeModule_.execute(
				CommonConcepts.DISJOINTWITH.getNode(dag_), 1);
		// Sort edges according to the maximum distance to THING node
		// Result of sorting will stored at toExplore_
		sortToExplore(disjointEdges);
		System.out.println("Pairs sorted");
		// Loop backwards
		int count = 0;
		int tenPercent = (disjointEdges.size()*2) / 10;
		for (int i = toExplore_.size() - 1; i > 0; i--) {
			System.out.println("Debug spot looping to explore");
			for (Pair<Node, Node> pair : toExplore_.get(i)) {
				// Find all Min parent nodes for source node
				System.out.println("Debug spot enter mingenls");
				Collection<Node> minGenls = CommonQuery.MINGENLS.runQuery(dag_,
						pair.objA_);
				System.out
						.println("Debug spot exit query mingenls and go loop");
				// Try to establish disjoint edge from (each parent of A) to (B)
				for (Node n : minGenls) {
					bubbleParentToTarget(creator, n, pair.objB_);
				}
				count++;
				if (count % tenPercent == 0)
					System.out.print((count / tenPercent * 10) + "% ");
			}
			System.out.println("Debug spot exit looping to explore");
		}
		System.out.println("Bubble up disjoints done");

		return false;

	}

	/**
	 * Sort the pairs from disjoint edges according to the distance from source
	 * side
	 * 
	 * For each pair added, the first node will be source and second node will
	 * be target. (For source node, parent nodes will be explored in attempt to
	 * establish disjoint with target node)
	 */
	private void sortToExplore(Collection<Edge> disjointEdges) {
		int temp;

		for (Edge e : disjointEdges) {
			// Get depth of first node and add the pair (Node1->Node2) to
			// appropriate location in the array

			// In the case of function, the depth of node become null
			if (((DAGNode) e.getNodes()[1]).getProperty("depth") != null) {
				temp = Integer.parseInt(((DAGNode) e.getNodes()[1])
						.getProperty("depth"));

				if (temp > ARRAYSIZE) {// TODO: display error message
				} else {
					toExplore_.get(temp).add(
							new Pair<Node, Node>(e.getNodes()[1],
									e.getNodes()[2]));
				}
			}

			// Second node, the pair is (Node2->Node1)
			if (((DAGNode) e.getNodes()[2]).getProperty("depth") != null) {
				temp = Integer.parseInt(((DAGNode) e.getNodes()[2])
						.getProperty("depth"));
				if (temp > ARRAYSIZE) {// TODO: display error message
				} else {
					toExplore_.get(temp).add(
							new Pair<Node, Node>(e.getNodes()[2],
									e.getNodes()[1]));
				}
			}
		}
	}


	/**
	 * Objective of this method is to prove disjoint between a parent node of a
	 * collection and a single target node. The method will go through each
	 * child node of the parent and check relationship with the target node. The
	 * method won't make change if any of these apply: 1, The pair of parent
	 * node and target node has been explored. 2, The parent node(or its parent)
	 * already has disjoint with the target node. 3, A child of the parent node
	 * found conjoint with target node.
	 * 
	 * collectionParent: The parent node exploredEdges: The hash set contains
	 * pairs of nodes that have been explored
	 */
	private void bubbleParentToTarget(Node creator, Node collectionParent,
			Node targetNode) {
		// If this is a sibling disjoint edge(parent of A genls B), ignore it.
		if (collectionParent.equals(targetNode))
			return;

		// If the pair of nodes has been explored before, skip
		if (exploredPairs_.contains(new Pair<Node, Node>(targetNode,
				collectionParent)))
			return;
		exploredPairs_.add(new Pair<Node, Node>(targetNode, collectionParent));
		// If the collectionParent is already disjoint with targetNode, skip it.
		System.out.println("Debug spot query module enter, have explored:"+exploredPairs_.size());
		if (queryModule_.prove(CommonConcepts.DISJOINTWITH.getNode(dag_),
				collectionParent, targetNode))
			return;
		System.out.println("Debug spot query module over common query enter");
		// Get all highest child nodes of collectionParent
		// System.out.println("Run query at line 526");
		List<Node> children = new ArrayList<Node>(
				CommonQuery.MAXSPECS.runQuery(dag_, collectionParent));
		System.out.println("Debug spot common query exit");
		int childrensize = children.size();
		if (childrensize == 0) {
			// System.out.println("no child");
			return;
		} else if (childrensize <= 10) {
			// System.out.println("Don't decide since children size is too small");
			return;
		}

		System.out.println(collectionParent.getName() + " has Children size:"
				+ childrensize + " ,target: " + targetNode.getName());

		int disjointcount = 0;
		int undefinedcount = 0;
		int randomsamplecount = 50;
		Random random = new Random();
		// random sampling for 100 times when children size is very large
		System.out.println("Debug spot enter looping child");
		for (int i = 0; i < childrensize; i++) {
			Node child;
			if (childrensize > 100) {
				child = children.get(random.nextInt(childrensize));
				randomsamplecount--;
				if (randomsamplecount == 0) {
					break;
				}
			} else {
				child = children.get(i);
			}
			System.out.println("Debug spot looping child+query:"
					+ child.getName());
			// if child disjoint with targetNode, count++
			// System.out.println("Run query at line 540");
			if (queryModule_.prove(CommonConcepts.DISJOINTWITH.getNode(dag_),
					targetNode, child)) {
				System.out.println("Debug spot query disjoint to:"
						+ targetNode.getName());
				disjointcount++;
			} else {
				// else if any conjoint found between the child and
				// targetNode,skip
				System.out.println("Debug spot transitiveModule_ to:"
						+ targetNode.getName());
				if (transitiveModule_.execute(true, targetNode, child) != null
						|| transitiveModule_.execute(false, targetNode, child) != null)
					return;
				else
					undefinedcount++;
			}
			// The impossibility of disjoint can be explored before loop
			// through all the child, since there is a statistic threshold
			if ((undefinedcount / childrensize) > 0.9) {
				return;
			}
			System.out.println("Debug spot looped one child");

		}
		System.out.println("Debug spot exit looping child");
		// Now use the count of disjoint found between collectionParent's
		// children and targetNode, and the amount of child to decide if
		// collectionParent disjoint with targetNode
		boolean isDisjointed = false;
		double p = (double) disjointcount / childrensize;
		System.out.println("p=" + p);

		// TODO: magic number here, need to be reasoned
		if (p > 0.4) {
			isDisjointed = true;
		}

		if (isDisjointed) {
			// If they are likely to be disjointed, create a new disjoint edge
			dag_.findOrCreateEdge(new Node[] {
					CommonConcepts.DISJOINTWITH.getNode(dag_),
					collectionParent, targetNode }, creator, false);
			System.out.println("Disjoint added btween:"
					+ collectionParent.getName() + " " + targetNode.getName());
		}
	}

}
