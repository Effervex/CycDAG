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
package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.StringNode;
import graph.module.OntologyEdgeModule;

import java.io.File;
import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class OntologyEdgeModuleTest {
	private OntologyEdgeModule sut_;
	private CycDAG dag_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (OntologyEdgeModule) dag_.getModule(OntologyEdgeModule.class);
		sut_.clear();
		CommonConcepts.initialise((CycDAG) dag_);
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
		sut_.clear();
	}

	@Test
	public void testFindEdgeByNodes() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true,
				false, true);
		DAGNode place = (DAGNode) dag_.findOrCreateNode("Place", creator, true,
				false, true);
		DAGNode diffFn = (DAGNode) dag_.findOrCreateNode(
				"CollectionDifferenceFn", creator, true, false, true);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true,
				false, true);
		DAGNode person = (DAGNode) dag_.findOrCreateNode("Person", creator,
				true, false, true);
		OntologyFunction diffThingPerson = dag_.findOrCreateFunctionNode(true,
				false, diffFn, thing, person);
		DAGEdge funcEdge = (DAGEdge) dag_.findOrCreateEdge(new Node[] { genls,
				place, diffThingPerson }, creator, true);
		Collection<Edge> results = sut_.findEdgeByNodes(genls, place,
				diffThingPerson);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(funcEdge));

		results = sut_.execute(diffThingPerson);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(funcEdge));

		// Non DAG nodes
		DAGNode cityNamedFn = (DAGNode) dag_.findOrCreateNode("CityNamedFn",
				creator, true, false, true);
		DAGNode nz = (DAGNode) dag_.findOrCreateNode("NewZealand", creator,
				true, false, true);
		OntologyFunction hamilton = dag_.findOrCreateFunctionNode(true, false,
				cityNamedFn, new StringNode("Hamilton"), nz);
		DAGEdge strEdge = (DAGEdge) dag_.findOrCreateEdge(new Node[] { genls,
				hamilton, place }, creator, true);
		results = sut_.findEdgeByNodes(genls, new OntologyFunction(cityNamedFn,
				new StringNode("Auckland"), nz), place);
		assertEquals(results.size(), 0);

		results = sut_.findEdgeByNodes(genls, new OntologyFunction(cityNamedFn,
				new StringNode("Hamilton"), nz));
		assertEquals(results.size(), 1);
		assertTrue(results.contains(strEdge));
	}

	@Test
	public void testStringFunction() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true,
				false, true);
		DAGNode fn = (DAGNode) dag_.findOrCreateNode("TheFn", creator, true,
				false, true);
		DAGNode blah = (DAGNode) dag_.findOrCreateNode("Blah", creator, true,
				false, true);
		DAGNode thingy = (DAGNode) dag_.findOrCreateNode("Thing", creator,
				true, false, true);
		OntologyFunction thingyFn = dag_.findOrCreateFunctionNode(true, false,
				fn, thingy, new StringNode("ABC"));
		DAGEdge funcEdge = (DAGEdge) dag_.findOrCreateEdge(new Node[] { genls,
				blah, thingyFn }, creator, true);
		OntologyFunction thingyFn2 = dag_.findOrCreateFunctionNode(true, false,
				fn, thingy, new StringNode("123"));
		DAGEdge funcEdge2 = (DAGEdge) dag_.findOrCreateEdge(new Node[] { genls,
				blah, thingyFn2 }, creator, true);
		Collection<Edge> results = sut_.execute(thingyFn2, 3);
		assertEquals(results.toString(), results.size(), 1);
		assertTrue(results.contains(funcEdge2));
	}

	@Test
	public void testRemovedEdges() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true,
				false, true);
		DAGNode isa = (DAGNode) dag_.findOrCreateNode("isa", creator, true,
				false, true);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true,
				false, true);
		DAGNode domesticated = (DAGNode) dag_.findOrCreateNode("Domesticated",
				creator, true, false, true);
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator,
				true, false, true);
		DAGEdge isaEdge = (DAGEdge) dag_.findOrCreateEdge(new Node[] { isa,
				dog, domesticated }, creator, true);
		DAGEdge genlsEdge = (DAGEdge) dag_.findOrCreateEdge(new Node[] { genls,
				dog, mammal }, creator, true);
		Collection<Edge> results = sut_.execute(dog);
		assertEquals(results.toString(), results.size(), 2);
		assertTrue(results.contains(isaEdge));
		assertTrue(results.contains(genlsEdge));

		results = sut_.execute(dog, isa, -1);
		assertEquals(results.toString(), results.size(), 1);
		assertTrue(results.contains(genlsEdge));

		results = sut_.execute(dog, genls, -1);
		assertEquals(results.toString(), results.size(), 1);
		assertTrue(results.contains(isaEdge));
	}
}
