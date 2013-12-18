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

import static org.junit.Assert.*;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.StringNode;
import graph.module.TransitiveIntervalSchemaModule;

import java.io.File;
import java.util.Collection;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TransitiveIntervalSchemaModuleTest {
	private CycDAG dag_;
	private TransitiveIntervalSchemaModule sut_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (TransitiveIntervalSchemaModule) dag_.getModule(TransitiveIntervalSchemaModule.class);
		sut_.clear();
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
		sut_.clear();
		dag_.noChecks_ = false;
	}

	@Test
	public void testTopologicalList() {
		Node creator = new StringNode("TestCreator");
		setUpDAG(creator);

		List<DAGNode> topSort = sut_.topologicalList(dag_.getNodes());
		Node dog = dag_.findOrCreateNode("Dog", creator, true);
		Node cat = dag_.findOrCreateNode("Cat", creator, true);
		Node canis = dag_.findOrCreateNode("Canis", creator, true);
		Node thing = dag_.findOrCreateNode("Thing", creator, true);
		Node pet = dag_.findOrCreateNode("Pet", creator, true);
		System.out.println(topSort);
		assertEquals(topSort.size(), 12);
		assertTrue(topSort.indexOf(dog) < topSort.indexOf(canis));
		assertTrue(topSort.indexOf(dog) < topSort.indexOf(thing));
		assertTrue(topSort.indexOf(dog) < topSort.indexOf(pet));
		assertTrue(topSort.indexOf(pet) < topSort.indexOf(thing));
		assertTrue(topSort.indexOf(canis) < topSort.indexOf(thing));
		assertTrue(topSort.indexOf(cat) < topSort.indexOf(thing));
	}

	@Test
	public void testFull() {
		Node creator = new StringNode("TestCreator");
		setUpDAG(creator);

		Node dog = dag_.findOrCreateNode("Dog", creator, true);
		Node cat = dag_.findOrCreateNode("Cat", creator, true);
		Node canis = dag_.findOrCreateNode("Canis", creator, true);
		Node mammal = dag_.findOrCreateNode("Mammal", creator, true);
		Node thing = dag_.findOrCreateNode("Thing", creator, true);
		Node pet = dag_.findOrCreateNode("Pet", creator, true);
		sut_.initialisationComplete(dag_.getNodes(), dag_.getEdges(), false);
		Collection<DAGNode> result = sut_.execute(true, dog, thing);
		assertNotNull(result);
		result = sut_.execute(false, thing, dog);
		assertNotNull(result);
		result = sut_.execute(true, dog, cat);
		assertNull(result);
		result = sut_.execute(false, cat, dog);
		assertNull(result);
		result = sut_.execute(true, canis, dog);
		assertNull(result);
		result = sut_.execute(false, dog, canis);
		assertNull(result);
		result = sut_.execute(true, cat, thing);
		assertNotNull(result);
		result = sut_.execute(false, thing, cat);
		assertNotNull(result);

		result = sut_.execute(true, dog);
		assertEquals(result.size(), 5);
		assertTrue(result.contains(dog));
		assertTrue(result.contains(canis));
		assertTrue(result.contains(thing));
		assertTrue(result.contains(pet));
		assertFalse(result.contains(cat));

		result = sut_.execute(true, thing);
		assertEquals(result.size(), 1);
		assertTrue(result.contains(thing));

		result = sut_.execute(false, thing);
		assertEquals(result.size(), 7);
		assertTrue(result.contains(dog));
		assertTrue(result.contains(canis));
		assertTrue(result.contains(thing));
		assertTrue(result.contains(pet));
		assertTrue(result.contains(cat));

		result = sut_.execute(false, mammal);
		assertEquals(result.size(), 4);
		assertTrue(result.contains(dog));
		assertTrue(result.contains(canis));
		assertFalse(result.contains(thing));
		assertFalse(result.contains(pet));
		assertTrue(result.contains(cat));
	}

	@Test
	public void testAddition() {
		Node creator = new StringNode("TestCreator");
		setUpDAG(creator);

		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true);
		DAGNode cat = (DAGNode) dag_.findOrCreateNode("Cat", creator, true);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true);
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator, true);
		DAGNode fluffy = (DAGNode) dag_.findOrCreateNode("Fluffy", creator, true);
		sut_.initialisationComplete(dag_.getNodes(), dag_.getEdges(), false);

		// Existing nodes
		dag_.noChecks_ = true;
		assertFalse(dag_.findOrCreateEdge(creator, new Node[] { genls, canis, fluffy }, false) instanceof ErrorEdge);
		assertNotNull(sut_.execute(true, canis, fluffy));
		assertNotNull(sut_.execute(true, dog, fluffy));
		assertTrue(sut_.execute(true, dog).contains(fluffy));
		assertFalse(sut_.execute(true, cat).contains(dog));
		assertFalse(sut_.execute(true, dog).contains(cat));
		assertFalse(sut_.execute(false, cat).contains(dog));
		assertFalse(sut_.execute(false, dog).contains(cat));
		assertTrue(sut_.execute(false, fluffy).contains(dog));

		// Tree addition (genls Mammal Animal)
		DAGNode animal = (DAGNode) dag_.findOrCreateNode("Animal", creator, true);
		assertFalse(dag_.findOrCreateEdge(creator, new Node[] { genls, mammal, animal }, false) instanceof ErrorEdge);
		assertNotNull(sut_.execute(true, mammal, animal));
		assertNotNull(sut_.execute(true, dog, animal));
		assertNotNull(sut_.execute(true, cat, animal));

		// Non-tree addition (genls Cow Mammal)
		DAGNode cow = (DAGNode) dag_.findOrCreateNode("Cow", creator, true);
		assertFalse(dag_.findOrCreateEdge(creator, new Node[] { genls, cow, mammal }, false) instanceof ErrorEdge);
		assertNotNull(sut_.execute(true, cow, mammal));
		assertNotNull(sut_.execute(true, cow, animal));
		assertNotNull(sut_.execute(true, cow, thing));
	}

	@Test
	public void testRemoval() {
		Node creator = new StringNode("TestCreator");
		setUpDAG(creator);

		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true);
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator, true);
		DAGNode pet = (DAGNode) dag_.findOrCreateNode("Pet", creator, true);
		sut_.initialisationComplete(dag_.getNodes(), dag_.getEdges(), false);

		// Removal of a tree edge
		dag_.noChecks_ = true;
		Edge edge = dag_.findEdge(genls, dog, canis);
		assertTrue(dag_.removeEdge(edge));
		assertNull(sut_.execute(true, dog, canis));
		assertNull(sut_.execute(true, dog, mammal));
		assertNotNull(sut_.execute(true, dog, pet));
		assertNotNull(sut_.execute(true, dog, thing));
		assertFalse(sut_.execute(false, canis).contains(dog));
		assertFalse(sut_.execute(false, mammal).contains(dog));
		assertTrue(sut_.execute(false, thing).contains(dog));

		// Removal of a non-tree edge
		edge = dag_.findEdge(genls, pet, thing);
		assertTrue(dag_.removeEdge(edge));
		assertNull(sut_.execute(true, pet, thing));
		assertNotNull(sut_.execute(true, dog, thing));
	}

	private void setUpDAG(Node creator) {
		Node genls = CommonConcepts.GENLS.getNode(dag_);
		dag_.noChecks_ = true;
		Node thing = dag_.findOrCreateNode("Thing", creator, true);
		Node dog = dag_.findOrCreateNode("Dog", creator, true);
		Node fluffy = dag_.findOrCreateNode("Fluffy", creator, true);
		Node mammal = dag_.findOrCreateNode("Mammal", creator, true);
		Node canis = dag_.findOrCreateNode("Canis", creator, true);
		Node pet = dag_.findOrCreateNode("Pet", creator, true);
		Node cat = dag_.findOrCreateNode("Cat", creator, true);
		Node plant = dag_.findOrCreateNode("Plant", creator, true);
		dag_.findOrCreateEdge(creator, new Node[] { genls, dog, thing }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, dog, canis }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, dog, pet }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, cat, pet }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, pet, thing }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, canis, mammal }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, plant, thing }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, cat, fluffy }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, mammal, thing }, false);
		dag_.findOrCreateEdge(creator, new Node[] { genls, cat, mammal }, false);
		dag_.noChecks_ = false;
		sut_.initMembers();
	}

}
