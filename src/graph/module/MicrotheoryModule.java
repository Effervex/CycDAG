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

import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGObject;
import graph.core.Edge;

import java.util.Collection;
import java.util.Set;

import util.collection.MultiMap;

public class MicrotheoryModule extends DAGModule<Collection<DAGEdge>> {
	private static final long serialVersionUID = -7361095570749061667L;

	private MultiMap<String, DAGEdge> microtheoryMap_;

	public MicrotheoryModule() {
		microtheoryMap_ = MultiMap.createConcurrentHashSetMultiMap();
	}

	@Override
	public Collection<DAGEdge> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		if (args.length != 1)
			return null;
		String microtheory = (String) args[0];

		Collection<DAGEdge> mtEdges = microtheoryMap_.get(microtheory);
		return mtEdges;
	}

	@Override
	public void clear() {
		microtheoryMap_.clear();
	}

	@Override
	public boolean addEdge(Edge edge) {
		if (edge instanceof DAGEdge) {
			DAGEdge dagEdge = (DAGEdge) edge;
			String edgeMt = dagEdge.getProperty(CycDAG.MICROTHEORY);
			if (edgeMt != null) {
				microtheoryMap_.put(edgeMt, dagEdge);
				return true;
			}
		}
		return false;
	}

	@Override
	public void addProperty(DAGObject dagObj, String key, String value) {
		if (key.equals(CycDAG.MICROTHEORY)) {
			if (dagObj instanceof DAGEdge) {
				DAGEdge dagEdge = (DAGEdge) dagObj;
				removeEdge(dagEdge);
				microtheoryMap_.put(value, dagEdge);
			}
		}
	}

	@Override
	public boolean removeEdge(Edge edge) {
		if (edge instanceof DAGEdge) {
			DAGEdge dagEdge = (DAGEdge) edge;
			String edgeMt = dagEdge.getProperty(CycDAG.MICROTHEORY);
			if (edgeMt != null) {
				try {
					Set<DAGEdge> edgeSet = microtheoryMap_
							.getConcurrentHashSet(edgeMt);
					if (edgeSet != null)
						edgeSet.remove(dagEdge);
				} catch (IllegalAccessException e) {
				}
				return true;
			}
		}
		return false;
	}

	@Override
	public void removeProperty(DAGObject dagObj, String key) {
		if (key.equals(CycDAG.MICROTHEORY)) {
			if (dagObj instanceof DAGEdge) {
				DAGEdge dagEdge = (DAGEdge) dagObj;
				removeEdge(dagEdge);
			}
		}
	}

	@Override
	public String toString() {
		return "Num MTs: " + microtheoryMap_.size();
	}
}
