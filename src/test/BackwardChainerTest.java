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

import static org.junit.Assert.assertNotNull;
import graph.core.DirectedAcyclicGraph;
import graph.inference.module.BackwardChainer;
import graph.module.QueryModule;

import java.io.File;

import org.junit.After;
import org.junit.Before;

public class BackwardChainerTest {
	private DirectedAcyclicGraph dag_;
	private BackwardChainer sut_;

	@Before
	public void setUp() throws Exception {
		dag_ = new DirectedAcyclicGraph(new File("test"));
		QueryModule queryModule = (QueryModule) dag_
				.getModule(QueryModule.class);
		assertNotNull(queryModule);
		sut_ = new BackwardChainer(queryModule);
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
	}

//	@Test
//	public void testProveQuery() {
//		DAGNode genls = new DAGNode("genls");
//		DAGNode dog = new DAGNode("Dog");
//		DAGNode canis = new DAGNode("CanisGenus");
//		assertFalse(sut_.query(genls, dog, canis).size() > 0);
//
//		// Add assertion
//		dag_.addEdge(new NamedEdge(genls, dog, canis));
//		assertTrue(sut_.query(genls, dog, canis).size() > 0);
//
//		// Add transitivity
//		DAGNode mammal = new DAGNode("Mammal");
//		dag_.addEdge(new NamedEdge(genls, canis, mammal));
//		VariableNode x = new VariableNode("?X");
//		VariableNode y = new VariableNode("?Y");
//		VariableNode z = new VariableNode("?Z");
//		HornClause transitivity = new HornClause(new Literal(genls, x, z),
//				new Literal(genls, x, y), new Literal(genls, y, z));
//		sut_.addRule(transitivity);
//		assertTrue(sut_.query(genls, dog, mammal).size() > 0);
//
//		// Further transitivity
//		DAGNode animal = new DAGNode("Animal");
//		dag_.addEdge(new NamedEdge(genls, mammal, animal));
//		assertTrue(sut_.query(genls, dog, animal).size() > 0);
//
//		DAGNode wolf = new DAGNode("Wolf");
//		dag_.addEdge(new NamedEdge(genls, wolf, canis));
//		assertTrue(sut_.query(genls, wolf, animal).size() > 0);
//		assertFalse(sut_.query(genls, wolf, dog).size() > 0);
//
//		// Self genls
//		HornClause selfGenls = new HornClause(new Literal(genls, x, x));
//		sut_.addRule(selfGenls);
//		assertTrue(sut_.query(genls, dog, dog).size() > 0);
//		assertTrue(sut_.query(genls, animal, animal).size() > 0);
//		assertTrue(sut_.query(genls, dog, animal).size() > 0);
//
//		// Isa clauses
//		DAGNode isa = new DAGNode("isa");
//		HornClause transIsa = new HornClause(new Literal(isa, x, z),
//				new Literal(isa, x, y), new Literal(genls, y, z));
//		sut_.addRule(transIsa);
//		DAGNode fido = new DAGNode("fido");
//		dag_.addEdge(new NamedEdge(isa, fido, dog));
//		assertTrue(sut_.query(isa, fido, dog).size() > 0);
//		assertTrue(sut_.query(isa, fido, mammal).size() > 0);
//		assertTrue(sut_.query(isa, fido, animal).size() > 0);
//
//		// 'Or' clause
//
//		// 'Not' clause
//
//		// Disjoint check
//		DAGNode disjoint = new DAGNode("disjointWith");
//		HornClause disjointRule = new HornClause(new Literal(disjoint, x, z),
//				new Literal(disjoint, x, y), new Literal(genls, z, y));
//		sut_.addRule(disjointRule);
//		HornClause disjointSymmetry = new HornClause(
//				new Literal(disjoint, x, y), new Literal(disjoint, y, x));
//		sut_.addRule(disjointSymmetry);
//		DAGNode plant = new DAGNode("plant");
//		dag_.addEdge(new NamedEdge(disjoint, animal, plant));
//		assertTrue(sut_.query(disjoint, animal, plant).size() > 0);
//		assertTrue(sut_.query(disjoint, plant, animal).size() > 0);
//		assertTrue(sut_.query(disjoint, dog, plant).size() > 0);
//		assertTrue(sut_.query(disjoint, wolf, plant).size() > 0);
//		assertFalse(sut_.query(disjoint, dog, animal).size() > 0);
//		assertFalse(sut_.query(disjoint, dog, wolf).size() > 0);
//		assertFalse(sut_.query(disjoint, dog, dog).size() > 0);
//
//		DAGNode tree = new DAGNode("tree");
//		dag_.addEdge(new NamedEdge(genls, tree, plant));
//		assertTrue(sut_.query(disjoint, animal, tree).size() > 0);
//		assertTrue(sut_.query(disjoint, tree, animal).size() > 0);
//		assertTrue(sut_.query(disjoint, dog, tree).size() > 0);
//	}
//
//	@Test
//	public void testAskQuery() throws Exception {
//		DAGNode genls = new DAGNode("genls");
//		DAGNode dog = new DAGNode("Dog");
//		DAGNode canis = new DAGNode("CanisGenus");
//		VariableNode x = new VariableNode("?X");
//		Collection<Substitution> results = sut_.query(genls, dog, x);
//		assertNull(results);
//
//		// Add assertion
//		results = sut_.query(genls, dog, canis);
//		assertNotNull(results);
//		assertEquals(results.size(), 1);
//		assertTrue(results.contains(new Substitution(x.toString(), canis)));
//
//		results = sut_.query(genls, x, dog);
//		assertNull(results);
//		results = sut_.query(genls, x, canis);
//		assertNotNull(results);
//		assertEquals(results.size(), 1);
//		assertTrue(results.contains(new Substitution(x.toString(), dog)));
//
//		// Add transitivity
//		DAGNode mammal = new DAGNode("Mammal");
//		dag_.addEdge(new NamedEdge(genls, canis, mammal));
//		VariableNode y = new VariableNode("?Y");
//		VariableNode z = new VariableNode("?Z");
//		HornClause transitivity = new HornClause(new Literal(genls, x, z),
//				new Literal(genls, x, y), new Literal(genls, y, z));
//		sut_.addRule(transitivity);
//		results = sut_.query(genls, dog, x);
//		assertNotNull(results);
//		assertEquals(results.size(), 2);
//		assertTrue(results.contains(new Substitution(x.toString(), canis)));
//		assertTrue(results.contains(new Substitution(x.toString(), mammal)));
//		results = sut_.query(genls, x, mammal);
//		assertNotNull(results);
//		assertEquals(results.size(), 2);
//		assertTrue(results.contains(new Substitution(x.toString(), canis)));
//		assertTrue(results.contains(new Substitution(x.toString(), dog)));
//
//		// Further transitivity
//		DAGNode animal = new DAGNode("Animal");
//		dag_.addEdge(new NamedEdge(genls, mammal, animal));
//		results = sut_.query(genls, dog, y);
//		assertNotNull(results);
//		assertEquals(results.size(), 3);
//		assertTrue(results.contains(new Substitution(y.toString(), canis)));
//		assertTrue(results.contains(new Substitution(y.toString(), mammal)));
//		assertTrue(results.contains(new Substitution(y.toString(), animal)));
//		results = sut_.query(genls, canis, y);
//		assertNotNull(results);
//		assertEquals(results.size(), 2);
//		assertTrue(results.contains(new Substitution(y.toString(), mammal)));
//		assertTrue(results.contains(new Substitution(y.toString(), animal)));
//
//		DAGNode wolf = new DAGNode("Wolf");
//		dag_.addEdge(new NamedEdge(genls, wolf, canis));
//		results = sut_.query(genls, dog, y);
//		assertNotNull(results);
//		assertEquals(results.size(), 3);
//		assertTrue(results.contains(new Substitution(y.toString(), canis)));
//		assertTrue(results.contains(new Substitution(y.toString(), mammal)));
//		assertTrue(results.contains(new Substitution(y.toString(), animal)));
//		results = sut_.query(genls, x, canis);
//		assertNotNull(results);
//		assertEquals(results.size(), 2);
//		assertTrue(results.contains(new Substitution(x.toString(), dog)));
//		assertTrue(results.contains(new Substitution(x.toString(), wolf)));
//
//		// Self genls
//		HornClause selfGenls = new HornClause(new Literal(genls, x, x));
//		sut_.addRule(selfGenls);
//		results = sut_.query(genls, dog, x);
//		assertNotNull(results);
//		assertEquals(results.size(), 4);
//		assertTrue(results.contains(new Substitution(x.toString(), dog)));
//		assertTrue(results.contains(new Substitution(x.toString(), canis)));
//		assertTrue(results.contains(new Substitution(x.toString(), mammal)));
//		assertTrue(results.contains(new Substitution(x.toString(), animal)));
//	}
}
