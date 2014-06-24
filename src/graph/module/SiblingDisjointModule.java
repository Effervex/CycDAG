/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *    Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.inference.VariableNode;

import java.util.Collection;

import org.apache.commons.collections4.CollectionUtils;

import util.collection.MultiMap;

public class SiblingDisjointModule extends DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 1L;
	private MultiMap<DAGNode, DAGNode> siblingDisjointParents_;

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		DAGNode node = (DAGNode) args[0];
		return getSiblingDisjointParents(node);
	}

	@SuppressWarnings("unchecked")
	public Collection<DAGNode> getSiblingDisjointParents(DAGNode node) {
		if (siblingDisjointParents_.containsKey(node))
			return siblingDisjointParents_.get(node);
		return CollectionUtils.EMPTY_COLLECTION;
	}

	@Override
	public boolean initialisationComplete(Collection<DAGNode> nodes,
			Collection<DAGEdge> edges, boolean forceRebuild) {
		if (isReady() && !forceRebuild)
			return false;
		QueryModule querier = (QueryModule) dag_.getModule(QueryModule.class);
		System.out
				.print("Building sibling disjoint collection shortcut map... ");

		// Find all sibling disjoint collection types
		VariableNode sibInst = new VariableNode("?X");
		VariableNode instColl = new VariableNode("?Y");
		QueryObject siblingDisjCollectionTypes = new QueryObject(
				CommonConcepts.AND.getNode(dag_), new OntologyFunction(
						CommonConcepts.ISA.getNode(dag_), sibInst,
						CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
								.getNode(dag_)), new OntologyFunction(
						CommonConcepts.ISA.getNode(dag_), instColl, sibInst));
		Collection<Substitution> siblingCollections = querier
				.execute(siblingDisjCollectionTypes);
		siblingDisjointParents_ = MultiMap.createConcurrentHashSetMultiMap();
		for (Substitution sub : siblingCollections) {
			Node sibColl = sub.getSubstitution(sibInst);
			Node collection = sub.getSubstitution(instColl);
			if (sibColl instanceof DAGNode && collection instanceof DAGNode)
				siblingDisjointParents_.put((DAGNode) collection,
						(DAGNode) sibColl);
		}
		System.out.println("Done!");

		return super.initialisationComplete(nodes, edges, forceRebuild);
	}

	public boolean isReady() {
		return siblingDisjointParents_ != null;
	}
}
