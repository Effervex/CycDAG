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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
import graph.core.StringNode;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.DateParseModule;
import graph.module.QueryModule;

import java.io.File;
import java.util.Collection;
import java.util.List;

import javax.naming.NamingException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class QueryModuleTest {
	private CycDAG dag_;
	private QueryModule sut_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (QueryModule) dag_.getModule(QueryModule.class);
		assertNotNull(sut_);
		CommonConcepts.initialise(dag_);
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
	}

	@Test
	public void testExecuteAnd() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true);
		DAGNode and = CommonConcepts.AND.getNode(dag_);
		dag_.findOrCreateEdge(new Node[] { genls, dog, canis }, creator, true);
		VariableNode x = VariableNode.DEFAULT;
		Collection<Substitution> results = sut_.execute(and,
				new OntologyFunction(genls, dog, x), new OntologyFunction(
						genls, dog, x));
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));

		results = sut_.execute(and, new OntologyFunction(genls, x, dog),
				new OntologyFunction(genls, dog, x));
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));

		DAGNode wolf = (DAGNode) dag_.findOrCreateNode("Wolf", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, wolf, canis }, creator, true);
		results = sut_.execute(and, new OntologyFunction(genls, x, dog),
				new OntologyFunction(genls, x, wolf));
		assertEquals(results.size(), 0);

		// Multiple variables
		VariableNode y = new VariableNode("?Y");
		results = sut_.execute(and, new OntologyFunction(genls, dog, x),
				new OntologyFunction(genls, wolf, y));
		assertEquals(results.size(), 4);
		Substitution s = new Substitution(x, dog);
		s.addSubstitution(y, wolf);
		assertTrue(results.contains(s));
		s = new Substitution(x, dog);
		s.addSubstitution(y, canis);
		assertTrue(results.contains(s));
		s = new Substitution(x, canis);
		s.addSubstitution(y, wolf);
		assertTrue(results.contains(s));
		s = new Substitution(x, canis);
		s.addSubstitution(y, canis);
		assertTrue(results.contains(s));

		// Different
		DAGNode different = CommonConcepts.DIFFERENT.getNode(dag_);
		results = sut_.execute(and, new OntologyFunction(genls, dog, x),
				new OntologyFunction(genls, wolf, y), new OntologyFunction(
						different, x, y));
		assertEquals(results.size(), 3);
		s = new Substitution(x, dog);
		s.addSubstitution(y, wolf);
		assertTrue(results.contains(s));
		s = new Substitution(x, dog);
		s.addSubstitution(y, canis);
		assertTrue(results.contains(s));
		s = new Substitution(x, canis);
		s.addSubstitution(y, wolf);
		assertTrue(results.contains(s));

		DAGNode isa = CommonConcepts.ISA.getNode(dag_);
		DAGNode species = (DAGNode) dag_.findOrCreateNode("BiologicalSpecies",
				creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, dog, species }, creator, true);
		results = sut_.execute(and, new OntologyFunction(isa, dog, x),
				new OntologyFunction(isa, dog, y), new OntologyFunction(
						different, x, y));
		assertEquals(results.size(), 0);

		// Equals
		DAGNode equals = CommonConcepts.EQUALS.getNode(dag_);
		results = sut_.execute(and, new OntologyFunction(genls, dog, x),
				new OntologyFunction(genls, wolf, y), new OntologyFunction(
						equals, x, y));
		assertEquals(results.size(), 1);
		s = new Substitution(x, canis);
		s.addSubstitution(y, canis);
		assertTrue(results.contains(s));

		results = sut_.execute(and, new OntologyFunction(genls, dog, x),
				new OntologyFunction(genls, wolf, canis));
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));

		// Isa
		DAGNode fido = (DAGNode) dag_.findOrCreateNode("Fido", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, fido, dog }, creator, true);
		results = sut_.execute(and, new OntologyFunction(isa, fido, x),
				new OntologyFunction(genls, wolf, x));
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, canis)));

		// Different variables
		results = sut_.execute(and, new OntologyFunction(isa, x, dog),
				new OntologyFunction(isa, y, canis));
		assertEquals(results.size(), 0);
	}

	@Test
	public void testExecuteDisjointWith() {
		Node creator = new StringNode("TestCreator");
		DAGNode disjoint = CommonConcepts.DISJOINTWITH.getNode(dag_);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true);
		DAGNode cat = (DAGNode) dag_.findOrCreateNode("Cat", creator, true);
		dag_.findOrCreateEdge(new Node[] { disjoint, cat, dog }, creator, true);
		QueryObject qo = new QueryObject(disjoint, cat, dog);
		assertNotNull(sut_.execute(qo));
		List<Node[]> justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0),
				new Node[] { disjoint, cat, dog });

		qo = new QueryObject(disjoint, dog, cat);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0),
				new Node[] { disjoint, cat, dog });

		// Variable
		VariableNode x = VariableNode.DEFAULT;
		qo = new QueryObject(disjoint, cat, x);
		Collection<Substitution> results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertEquals(results, sut_.execute(disjoint, x, cat));

		qo = new QueryObject(disjoint, dog, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, cat)));
		assertEquals(results, sut_.execute(disjoint, x, dog));

		// Single drop down
		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		DAGNode boxer = (DAGNode) dag_.findOrCreateNode("Boxer-Dog", creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, boxer, dog }, creator, true);
		qo = new QueryObject(disjoint, boxer, cat);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, boxer, dog });
		assertArrayEquals(justification.get(1),
				new Node[] { disjoint, cat, dog });

		qo = new QueryObject(disjoint, cat, boxer);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0),
				new Node[] { disjoint, cat, dog });
		assertArrayEquals(justification.get(1),
				new Node[] { genls, boxer, dog });

		qo = new QueryObject(disjoint, dog, boxer);
		assertNull(sut_.execute(qo));
		qo = new QueryObject(disjoint, boxer, dog);
		assertNull(sut_.execute(qo));

		// Variable
		qo = new QueryObject(disjoint, cat, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertEquals(results, sut_.execute(disjoint, x, cat));

		// Common super-collection
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, dog, mammal }, creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, cat, mammal }, creator, true);
		qo = new QueryObject(disjoint, boxer, mammal);
		assertNull(sut_.execute(qo));
		qo = new QueryObject(disjoint, dog, mammal);
		assertNull(sut_.execute(qo));
		qo = new QueryObject(disjoint, cat, mammal);
		assertNull(sut_.execute(qo));

		qo = new QueryObject(disjoint, mammal, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 0);

		// Uncommon super-collection
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, dog, canis }, creator, true);
		qo = new QueryObject(disjoint, canis, cat);
		assertNull(sut_.execute(qo));
		dag_.findOrCreateEdge(new Node[] { genls, canis, mammal }, creator,
				true);

		qo = new QueryObject(disjoint, canis, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 0);
		qo = new QueryObject(disjoint, cat, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertEquals(results, sut_.execute(disjoint, x, cat));

		// Sub-collection
		DAGNode tomcat = (DAGNode) dag_.findOrCreateNode("TomCat", creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, tomcat, cat }, creator, true);
		qo = new QueryObject(disjoint, tomcat, dog);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, tomcat, cat });
		assertArrayEquals(justification.get(1),
				new Node[] { disjoint, cat, dog });

		qo = new QueryObject(disjoint, tomcat, boxer);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 3);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, tomcat, cat });
		assertArrayEquals(justification.get(1),
				new Node[] { disjoint, cat, dog });
		assertArrayEquals(justification.get(2),
				new Node[] { genls, boxer, dog });

		qo = new QueryObject(disjoint, tomcat, cat);
		assertNull(sut_.execute(qo));

		qo = new QueryObject(disjoint, tomcat, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));

		DAGNode living = (DAGNode) dag_.findOrCreateNode("LivingThing",
				creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, mammal, living }, creator,
				true);
		DAGNode nonliving = (DAGNode) dag_.findOrCreateNode("NonLivingThing",
				creator, true);
		DAGNode book = (DAGNode) dag_.findOrCreateNode("Book", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, book, nonliving }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { disjoint, living, nonliving },
				creator, true);
		qo = new QueryObject(disjoint, book, cat);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 4);
		assertArrayEquals(justification.get(0), new Node[] { genls, book,
				nonliving });
		assertArrayEquals(justification.get(1), new Node[] { disjoint, living,
				nonliving });
		assertArrayEquals(justification.get(2), new Node[] { genls, mammal,
				living });
		assertArrayEquals(justification.get(3),
				new Node[] { genls, cat, mammal });

		qo = new QueryObject(disjoint, book, boxer);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 5);
		assertArrayEquals(justification.get(0), new Node[] { genls, book,
				nonliving });
		assertArrayEquals(justification.get(1), new Node[] { disjoint, living,
				nonliving });
		assertArrayEquals(justification.get(2), new Node[] { genls, mammal,
				living });
		assertArrayEquals(justification.get(3),
				new Node[] { genls, dog, mammal });
		assertArrayEquals(justification.get(4),
				new Node[] { genls, boxer, dog });

		qo = new QueryObject(disjoint, book, mammal);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 3);
		assertArrayEquals(justification.get(0), new Node[] { genls, book,
				nonliving });
		assertArrayEquals(justification.get(1), new Node[] { disjoint, living,
				nonliving });
		assertArrayEquals(justification.get(2), new Node[] { genls, mammal,
				living });

		qo = new QueryObject(disjoint, book, canis);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 4);
		assertArrayEquals(justification.get(0), new Node[] { genls, book,
				nonliving });
		assertArrayEquals(justification.get(1), new Node[] { disjoint, living,
				nonliving });
		assertArrayEquals(justification.get(2), new Node[] { genls, mammal,
				living });
		assertArrayEquals(justification.get(3), new Node[] { genls, canis,
				mammal });

		qo = new QueryObject(disjoint, book, nonliving);
		assertNull(sut_.execute(qo));

		qo = new QueryObject(disjoint, tomcat, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, nonliving)));

		qo = new QueryObject(disjoint, tomcat, boxer);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 3);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, tomcat, cat });
		assertArrayEquals(justification.get(1),
				new Node[] { disjoint, cat, dog });
		assertArrayEquals(justification.get(2),
				new Node[] { genls, boxer, dog });

		// SiblingDisjointCollectionType
		Edge edge = dag_.findOrCreateEdge(new Node[] { disjoint, cat, dog },
				creator, false);
		dag_.removeEdge(edge);
		qo = new QueryObject(disjoint, cat, dog);
		assertNull(sut_.execute(qo));
		qo = new QueryObject(disjoint, dog, cat);
		assertNull(sut_.execute(qo));

		DAGNode species = (DAGNode) dag_.findOrCreateNode("BiologicalSpecies",
				creator, true);
		DAGNode isa = CommonConcepts.ISA.getNode(dag_);
		dag_.findOrCreateEdge(new Node[] { isa, dog, species }, creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, cat, species }, creator, true);
		DAGNode disjointCollectionType = (DAGNode) dag_.findOrCreateNode(
				"DisjointCollectionType", creator, true);
		DAGNode siblingDisjointCollectionType = CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
				.getNode(dag_);
		dag_.findOrCreateEdge(
				new Node[] { isa, species, disjointCollectionType }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, disjointCollectionType,
				siblingDisjointCollectionType }, creator, true);
		qo = new QueryObject(disjoint, cat, dog);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 6);
		assertArrayEquals(justification.get(0),
				new Node[] { isa, cat, species });
		assertArrayEquals(justification.get(1), new Node[0]);
		assertArrayEquals(justification.get(2),
				new Node[] { isa, dog, species });
		assertArrayEquals(justification.get(3), new Node[0]);
		assertArrayEquals(justification.get(4), new Node[] { isa, species,
				disjointCollectionType });
		assertArrayEquals(justification.get(5), new Node[] { genls,
				disjointCollectionType, siblingDisjointCollectionType });

		qo = new QueryObject(disjoint, dog, cat);
		assertNotNull(sut_.execute(qo));

		qo = new QueryObject(disjoint, cat, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, nonliving)));
		qo = new QueryObject(disjoint, dog, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, cat)));
		assertTrue(results.contains(new Substitution(x, nonliving)));

		DAGNode sibDisjExcep = CommonConcepts.SIBLING_DISJOINT_EXCEPTION
				.getNode(dag_);
		DAGNode symmetric = CommonConcepts.SYMMETRIC_BINARY.getNode(dag_);
		dag_.findOrCreateEdge(new Node[] { sibDisjExcep, cat, dog }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { isa, sibDisjExcep, symmetric },
				creator, true);
		qo = new QueryObject(disjoint, cat, dog);
		assertNull(sut_.execute(qo));
		qo = new QueryObject(disjoint, cat, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, nonliving)));
		qo = new QueryObject(disjoint, x, cat);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, nonliving)));
		qo = new QueryObject(disjoint, dog, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, nonliving)));
		qo = new QueryObject(disjoint, x, dog);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, nonliving)));

		// Curious case of transitivity
		edge = dag_.findOrCreateEdge(new Node[] { disjoint, cat, boxer },
				creator, false);
		dag_.removeEdge(edge);
		assertTrue(sut_.prove(genls, boxer, dog));
		assertTrue(sut_.prove(isa, dog, species));
		assertTrue(sut_.prove(isa, cat, species));
		qo = new QueryObject(disjoint, boxer, cat);
		assertNotNull(sut_.execute(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 8);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, boxer, dog });
		assertArrayEquals(justification.get(1), new Node[0]);
		assertArrayEquals(justification.get(2),
				new Node[] { isa, dog, species });
		assertArrayEquals(justification.get(3), new Node[0]);
		assertArrayEquals(justification.get(4),
				new Node[] { isa, cat, species });
		assertArrayEquals(justification.get(5), new Node[0]);
		assertArrayEquals(justification.get(6), new Node[] { isa, species,
				disjointCollectionType });
		assertArrayEquals(justification.get(7), new Node[] { genls,
				disjointCollectionType, siblingDisjointCollectionType });
		
		qo = new QueryObject(disjoint, boxer, dog);
		assertNull(sut_.execute(qo));
	}

	@Test
	public void testExecuteGenls() throws NamingException {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, dog, canis }, creator, true);
		VariableNode x = VariableNode.DEFAULT;
		QueryObject qo = new QueryObject(genls, dog, x);
		Collection<Substitution> results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));

		Collection<Node> minGenls = CommonQuery.MINGENLS.runQuery(dag_, dog);
		assertTrue(minGenls.contains(canis));
		assertEquals(minGenls.size(), 1);

		// Proofs
		qo = new QueryObject(genls, dog, canis);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		List<Node[]> justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, dog, canis });

		qo = new QueryObject(genls, dog, dog);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0), new Node[] { genls, dog, dog });

		qo = new QueryObject(genls, canis, canis);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0), new Node[] { genls, canis,
				canis });

		qo = new QueryObject(genls, canis, dog);
		results = sut_.execute(qo);
		assertNull(results);

		qo = new QueryObject(genls, x, dog);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));

		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, canis, mammal }, creator,
				true);
		qo = new QueryObject(genls, dog, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 3);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));
		assertTrue(results.contains(new Substitution(x, mammal)));

		qo = new QueryObject(genls, dog, mammal);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, dog, canis });
		assertArrayEquals(justification.get(1), new Node[] { genls, canis,
				mammal });

		minGenls = CommonQuery.MINGENLS.runQuery(dag_, dog);
		assertTrue(minGenls.contains(canis));
		assertEquals(minGenls.size(), 1);

		dag_.findOrCreateEdge(new Node[] { genls, dog, mammal }, creator, true);
		minGenls = CommonQuery.MINGENLS.runQuery(dag_, dog);
		assertTrue(minGenls.contains(canis));
		assertEquals(minGenls.size(), 1);

		qo = new QueryObject(genls, x, mammal);
		results = sut_.execute(qo);
		assertEquals(results.size(), 3);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));
		assertTrue(results.contains(new Substitution(x, mammal)));

		// Proofs
		qo = new QueryObject(genls, dog, mammal);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, dog, mammal });

		DAGNode wolf = (DAGNode) dag_.findOrCreateNode("Wolf", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, wolf, canis }, creator, true);
		qo = new QueryObject(genls, wolf, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 3);
		assertTrue(results.contains(new Substitution(x, wolf)));
		assertTrue(results.contains(new Substitution(x, canis)));
		assertTrue(results.contains(new Substitution(x, mammal)));

		VariableNode y = new VariableNode("?Y");
		qo = new QueryObject(genls, y, x);
		results = sut_.execute(qo);
		assertTrue(results.isEmpty());

		// Higher
		DAGNode animal = (DAGNode) dag_.findOrCreateNode("Animal", creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, mammal, animal }, creator,
				true);
		qo = new QueryObject(genls, wolf, mammal);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, wolf, canis });
		assertArrayEquals(justification.get(1), new Node[] { genls, canis,
				mammal });

		qo = new QueryObject(genls, wolf, animal);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 3);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, wolf, canis });
		assertArrayEquals(justification.get(1), new Node[] { genls, canis,
				mammal });
		assertArrayEquals(justification.get(2), new Node[] { genls, mammal,
				animal });

		// Shortest justification
		dag_.findOrCreateEdge(new Node[] { genls, canis, animal }, creator,
				true);
		qo = new QueryObject(genls, wolf, animal);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0),
				new Node[] { genls, wolf, canis });
		assertArrayEquals(justification.get(1), new Node[] { genls, canis,
				animal });

	}

	@Test
	public void testExecuteGenlsStress() {
		Node creator = new StringNode("TestCreator");
		VariableNode x = VariableNode.DEFAULT;
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true);
		QueryObject qo;
		Collection<Substitution> results;
		Collection<Node> minGenls;
		// Avoiding loops
		DAGNode a = (DAGNode) dag_.findOrCreateNode("Aaaa", creator, true);
		DAGNode prior = null;
		int repeats = 100;
		for (int i = 1; i <= repeats; i++) {
			DAGNode b = (DAGNode) dag_.findOrCreateNode("Bbbb" + i, creator,
					true);
			if (prior != null)
				dag_.findOrCreateEdge(new Node[] { genls, prior, b }, creator,
						true);
			dag_.findOrCreateEdge(new Node[] { genls, a, b }, creator, true);
			prior = b;
		}
		long start = System.currentTimeMillis();
		qo = new QueryObject(genls, a, x);
		results = sut_.execute(qo);
		long end = System.currentTimeMillis();
		assertEquals(results.size(), repeats + 1);
		long elapsed = end - start;
		System.out.println(elapsed);
		assertTrue(elapsed + "", elapsed < .1 * repeats);

		minGenls = CommonQuery.MINGENLS.runQuery(dag_, a);
		assertEquals(minGenls.size(), 1);

		start = System.currentTimeMillis();
		qo = new QueryObject(genls, x, prior);
		results = sut_.execute(qo);
		end = System.currentTimeMillis();
		assertEquals(results.size(), repeats + 1);
		elapsed = end - start;
		System.out.println(elapsed);
		assertTrue(elapsed + "", elapsed < .1 * repeats);
	}

	@Test
	public void testPredicateTransitivity() throws NamingException {
		// TODO Not actually sure if predicates are transitive...
		Node creator = new StringNode("TestCreator");
		DAGNode genlPreds = (DAGNode) dag_.findOrCreateNode("genlPreds",
				creator, true);
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true);
		DAGNode capitalCity = (DAGNode) dag_.findOrCreateNode("capitalCity",
				creator, true);
		DAGNode geoSub = (DAGNode) dag_.findOrCreateNode(
				"geopoliticalSubdivision", creator, true);
		dag_.findOrCreateEdge(new Node[] { genlPreds, capitalCity, geoSub },
				creator, true);
		VariableNode x = VariableNode.DEFAULT;
		QueryObject qo = new QueryObject(genlPreds, capitalCity, x);
		Collection<Substitution> results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, capitalCity)));
		assertTrue(results.contains(new Substitution(x, geoSub)));

		DAGNode capitalCityState = (DAGNode) dag_.findOrCreateNode(
				"capitalCityOfState", creator, true);
		dag_.findOrCreateEdge(new Node[] { genlPreds, capitalCityState,
				capitalCity }, creator, true);
		qo = new QueryObject(genlPreds, capitalCityState, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 3);
		assertTrue(results.contains(new Substitution(x, capitalCityState)));
		assertTrue(results.contains(new Substitution(x, capitalCity)));
		assertTrue(results.contains(new Substitution(x, geoSub)));

		// genlPreds transitivity
		DAGNode texas = (DAGNode) dag_.findOrCreateNode("Texas-State", creator,
				true);
		DAGNode austin = (DAGNode) dag_.findOrCreateNode("CityOfAustinTX",
				creator, true);
		Edge e = dag_.findOrCreateEdge(new Node[] { capitalCityState, texas,
				austin }, creator, true);
		assertFalse(e instanceof ErrorEdge);
		qo = new QueryObject(capitalCityState, texas, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, austin)));
		qo = new QueryObject(capitalCity, texas, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, austin)));

		DAGNode arg1Isa = (DAGNode) dag_.findOrCreateNode("arg1Isa", creator,
				true);
		DAGNode geoEntity = (DAGNode) dag_.findOrCreateNode(
				"GeopoliticalEntity", creator, true);
		DAGNode geoRegion = (DAGNode) dag_.findOrCreateNode(
				"GeographicalRegion", creator, true);
		e = dag_.findOrCreateEdge(
				new Node[] { arg1Isa, capitalCity, geoEntity }, creator, true);
		assertFalse(e instanceof ErrorEdge);
		e = dag_.findOrCreateEdge(new Node[] { genls, geoEntity, geoRegion },
				creator, true);
		assertFalse(e instanceof ErrorEdge);
		qo = new QueryObject(arg1Isa, capitalCity, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, geoEntity)));

		qo = new QueryObject(arg1Isa, capitalCityState, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, geoEntity)));

		qo = new QueryObject(arg1Isa, capitalCityState, geoRegion);
		results = sut_.execute(qo);
		assertNotNull(results);

		DAGNode argIsa = (DAGNode) dag_.findOrCreateNode("argIsa", creator,
				true);
		DAGNode capCityCol = (DAGNode) dag_.findOrCreateNode(
				"CapitalCityOfRegion", creator, true);
		dag_.findOrCreateEdge(
				new Node[] { argIsa, capitalCity, PrimitiveNode.parseNode("2"),
						capCityCol }, creator, true);
		qo = new QueryObject(argIsa, capitalCity, PrimitiveNode.parseNode("2"),
				x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, capCityCol)));
	}

	@Test
	public void testExecuteIsa() throws NamingException {
		Node creator = new StringNode("TestCreator");
		DAGNode isa = (DAGNode) dag_.findOrCreateNode("isa", creator, true);
		DAGNode fido = (DAGNode) dag_.findOrCreateNode("Fido", creator, true);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, fido, dog }, creator, true);
		VariableNode x = VariableNode.DEFAULT;
		QueryObject qo = new QueryObject(isa, fido, x);
		Collection<Substitution> results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, dog)));

		Collection<Node> minIsa = CommonQuery.MINISA.runQuery(dag_, fido);
		assertTrue(minIsa.contains(dog));
		assertEquals(minIsa.size(), 1);

		qo = new QueryObject(isa, x, dog);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, fido)));

		qo = new QueryObject(isa, dog, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 0);

		// Proof
		qo = new QueryObject(isa, fido, dog);
		results = sut_.execute(qo);
		assertNotNull(results);
		List<Node[]> justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0), new Node[] { isa, fido, dog });

		qo = new QueryObject(isa, dog, fido);
		results = sut_.execute(qo);
		assertNull(results);

		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, dog, canis }, creator, true);
		qo = new QueryObject(isa, fido, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));

		minIsa = CommonQuery.MINISA.runQuery(dag_, fido);
		assertTrue(minIsa.contains(dog));
		assertEquals(minIsa.size(), 1);

		qo = new QueryObject(isa, x, canis);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, fido)));

		// Proof
		qo = new QueryObject(isa, fido, dog);
		results = sut_.execute(qo);
		assertNotNull(results);

		qo = new QueryObject(isa, fido, canis);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { isa, fido, dog });
		assertArrayEquals(justification.get(1),
				new Node[] { genls, dog, canis });

		qo = new QueryObject(isa, dog, canis);
		results = sut_.execute(qo);
		assertNull(results);

		DAGNode species = (DAGNode) dag_.findOrCreateNode("BiologicalSpecies",
				creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, canis, species }, creator, true);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, species, thing }, creator,
				true);

		qo = new QueryObject(isa, fido, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, dog)));
		assertTrue(results.contains(new Substitution(x, canis)));

		minIsa = CommonQuery.MINISA.runQuery(dag_, fido);
		assertTrue(minIsa.contains(dog));
		assertEquals(minIsa.size(), 1);

		qo = new QueryObject(isa, canis, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, species)));
		assertTrue(results.contains(new Substitution(x, thing)));

		minIsa = CommonQuery.MINISA.runQuery(dag_, canis);
		assertTrue(minIsa.contains(species));
		assertEquals(minIsa.size(), 1);

		qo = new QueryObject(isa, dog, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 0);

		qo = new QueryObject(isa, x, species);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);
		assertTrue(results.contains(new Substitution(x, canis)));

		// Proofs
		qo = new QueryObject(isa, canis, species);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0), new Node[] { isa, canis,
				species });

		qo = new QueryObject(isa, dog, species);
		results = sut_.execute(qo);
		assertNull(results);
		qo = new QueryObject(isa, dog, thing);
		results = sut_.execute(qo);
		assertNull(results);
		qo = new QueryObject(isa, fido, species);
		results = sut_.execute(qo);
		assertNull(results);

		DAGNode wolf = (DAGNode) dag_.findOrCreateNode("Wolf", creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, wolf, canis }, creator, true);
		DAGNode growls = (DAGNode) dag_.findOrCreateNode("Growls", creator,
				true);
		dag_.findOrCreateEdge(new Node[] { isa, growls, wolf }, creator, true);
		qo = new QueryObject(isa, growls, x);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, wolf)));
		assertTrue(results.contains(new Substitution(x, canis)));

		qo = new QueryObject(isa, x, canis);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution(x, growls)));
		assertTrue(results.contains(new Substitution(x, fido)));

		// Proofs
		qo = new QueryObject(isa, wolf, species);
		results = sut_.execute(qo);
		assertNull(results);
		qo = new QueryObject(isa, growls, species);
		results = sut_.execute(qo);
		assertNull(results);

		// Multiple isas
		DAGNode pet = (DAGNode) dag_.findOrCreateNode("Pet", creator, true);
		DAGNode domesticated = (DAGNode) dag_.findOrCreateNode("Domesticated",
				creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, pet, domesticated }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { isa, fido, pet }, creator, true);
		qo = new QueryObject(isa, fido, canis);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { isa, fido, dog });
		assertArrayEquals(justification.get(1),
				new Node[] { genls, dog, canis });

		qo = new QueryObject(isa, fido, domesticated);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { isa, fido, pet });
		assertArrayEquals(justification.get(1), new Node[] { genls, pet,
				domesticated });
	}

	@Test
	public void testFunctionTransitivity() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true);
		DAGNode fruitFn = (DAGNode) dag_.findOrCreateNode("FruitFn", creator,
				true);
		DAGNode fruit = (DAGNode) dag_.findOrCreateNode("Fruit", creator, true);
		DAGNode appleTree = (DAGNode) dag_.findOrCreateNode("AppleTree",
				creator, true);
		DAGNode resultGenl = (DAGNode) dag_.findOrCreateNode("resultGenl",
				creator, true);
		QueryObject qo;
		Collection<Substitution> results;

		assertEdge(dag_.findOrCreateEdge(new Node[] { resultGenl, fruitFn,
				fruit }, creator, true));
		DAGNode apple = (DAGNode) dag_.findOrCreateNode("(FruitFn AppleTree)",
				creator, true);
		qo = new QueryObject(genls, apple, fruit);
		results = sut_.execute(qo);
		assertNotNull(results);
		List<Node[]> justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0), new Node[] { genls, apple,
				fruit });

		// Genls
		DAGNode plant = (DAGNode) dag_.findOrCreateNode("Plant", creator, true);
		assertEdge(dag_.findOrCreateEdge(new Node[] { genls, fruit, plant },
				creator, true));
		qo = new QueryObject(genls, apple, plant);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { genls, apple,
				fruit });
		assertArrayEquals(justification.get(1), new Node[] { genls, fruit,
				plant });

		qo = new QueryObject(genls, apple, VariableNode.DEFAULT);
		results = sut_.execute(qo);
		assertEquals(results.size(), 2);
		assertTrue(results.contains(new Substitution("?X", fruit)));
		assertTrue(results.contains(new Substitution("?X", plant)));

		DAGNode isa = (DAGNode) dag_.findOrCreateNode("isa", creator, true);
		DAGNode fatherFn = (DAGNode) dag_.findOrCreateNode("FatherFn", creator,
				true);
		DAGNode resultIsa = (DAGNode) dag_.findOrCreateNode("resultIsa",
				creator, true);
		DAGNode sam = (DAGNode) dag_.findOrCreateNode("Sam", creator, true);
		DAGNode person = (DAGNode) dag_.findOrCreateNode("Person", creator,
				true);

		assertEdge(dag_.findOrCreateEdge(new Node[] { resultIsa, fatherFn,
				person }, creator, true));
		DAGNode samDad = (DAGNode) dag_.findOrCreateNode("(FatherFn Sam)",
				creator, true);
		qo = new QueryObject(isa, samDad, person);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);
		assertArrayEquals(justification.get(0), new Node[] { isa, samDad,
				person });

		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true);
		assertEdge(dag_.findOrCreateEdge(new Node[] { genls, person, thing },
				creator, true));
		qo = new QueryObject(isa, samDad, thing);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { isa, samDad,
				person });
		assertArrayEquals(justification.get(1), new Node[] { genls, person,
				thing });

		// Isa
		Node myApple = dag_.findOrCreateNode("SamsApple", creator, true);
		assertEdge(dag_.findOrCreateEdge(new Node[] { isa, myApple, apple },
				creator, true));
		qo = new QueryObject(isa, myApple, fruit);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { isa, myApple,
				apple });
		assertArrayEquals(justification.get(1), new Node[] { genls, apple,
				fruit });

		qo = new QueryObject(isa, myApple, VariableNode.DEFAULT);
		results = sut_.execute(qo);
		assertEquals(results.size(), 3);
		assertTrue(results.contains(new Substitution("?X", apple)));
		assertTrue(results.contains(new Substitution("?X", fruit)));
		assertTrue(results.contains(new Substitution("?X", plant)));

		// Disjoint
		DAGNode disjoint = (DAGNode) dag_.findOrCreateNode("disjointWith",
				creator, true);
		Node vege = dag_.findOrCreateNode("Vegetable", creator, true);
		assertEdge(dag_.findOrCreateEdge(new Node[] { genls, vege, plant },
				creator, true));
		assertEdge(dag_.findOrCreateEdge(new Node[] { disjoint, fruit, vege },
				creator, true));
		qo = new QueryObject(disjoint, apple, vege);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { genls, apple,
				fruit });
		assertArrayEquals(justification.get(1), new Node[] { disjoint, fruit,
				vege });

		// Recursive definition
		Node hardcore = dag_.findOrCreateNode("HardcoreFn", creator, true);
		Node hardcorePlant = dag_.findOrCreateNode("(HardcoreFn Plant)",
				creator, true);
		assertEdge(dag_.findOrCreateEdge(new Node[] { resultGenl, hardcore,
				hardcorePlant }, creator, true));
		qo = new QueryObject(genls, hardcorePlant, VariableNode.DEFAULT);
		results = sut_.execute(qo);
		assertEquals(results.size(), 1);

		// Justification check
		dag_.findOrCreateEdge(new Node[] { genls, fruit, fruit }, creator, true);
		Node redapple = dag_.findOrCreateNode("RedApple", creator, true);
		assertEdge(dag_.findOrCreateEdge(new Node[] { isa, redapple, apple },
				creator, true));
		assertEdge(dag_.findOrCreateEdge(new Node[] { genls, redapple, apple },
				creator, true));
		qo = new QueryObject(genls, redapple, fruit);
		results = sut_.execute(qo);
		assertNotNull(results);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);
		assertArrayEquals(justification.get(0), new Node[] { genls, redapple,
				apple });
		assertArrayEquals(justification.get(1), new Node[] { genls, apple,
				fruit });
	}

	private void assertEdge(Edge edge) {
		assertFalse(edge.toString(), edge instanceof ErrorEdge);
	}

	@Test
	public void testExecuteIsaStress() {
		Node creator = new StringNode("TestCreator");
		VariableNode x = VariableNode.DEFAULT;
		DAGNode isa = (DAGNode) dag_.findOrCreateNode("isa", creator, true);
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true);
		QueryObject qo;
		Collection<Substitution> results;

		// Avoiding loops
		DAGNode a = (DAGNode) dag_.findOrCreateNode("Aaaa", creator, true);
		DAGNode prior = null;
		int repeats = 100;
		for (int i = 1; i <= repeats; i++) {
			DAGNode b = (DAGNode) dag_.findOrCreateNode("Bbbb" + i, creator,
					true);
			if (prior != null)
				dag_.findOrCreateEdge(new Node[] { genls, prior, b }, creator,
						true);
			dag_.findOrCreateEdge(new Node[] { isa, a, b }, creator, true);
			prior = b;
		}
		long start = System.currentTimeMillis();
		qo = new QueryObject(isa, a, x);
		results = sut_.execute(qo);
		long end = System.currentTimeMillis();
		assertEquals(results.size(), repeats);
		long elapsed = end - start;
		System.out.println(elapsed);
		// assertTrue(elapsed + "", elapsed < .1 * repeats);

		start = System.currentTimeMillis();
		qo = new QueryObject(isa, x, prior);
		results = sut_.execute(qo);
		end = System.currentTimeMillis();
		assertEquals(results.size(), 1);
		elapsed = end - start;
		System.out.println(elapsed);
		// assertTrue(elapsed + "", elapsed < .5 * repeats);
	}

	@Test
	public void testLaterThan() {
		DateParseModule dpm = (DateParseModule) dag_
				.getModule(DateParseModule.class);
		Node laterThan = CommonConcepts.LATER_PREDICATE.getNode(dag_);
		Node dateA = dpm.parseDate("2000").iterator().next();
		Node dateB = dpm.parseDate("1999").iterator().next();
		QueryObject qo = new QueryObject(laterThan, dateA, dateB);
		assertNotNull(sut_.execute(qo));

		dateA = dpm.parseDate("2000").iterator().next();
		dateB = dpm.parseDate("2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNull(sut_.execute(qo));

		Node creator = new StringNode("TestCreator");
		dag_.findOrCreateNode("April", creator, true);
		dag_.findOrCreateNode("March", creator, true);
		dateA = dpm.parseDate("April 2000").iterator().next();
		dateB = dpm.parseDate("March 2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNotNull(sut_.execute(qo));

		dag_.findOrCreateNode("May", creator, true);
		dateB = dpm.parseDate("May 2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNull(sut_.execute(qo));

		dateA = dpm.parseDate("4 April 2000").iterator().next();
		dateB = dpm.parseDate("3 April 2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNotNull(sut_.execute(qo));

		dateA = dpm.parseDate("4 April 2000").iterator().next();
		dateB = dpm.parseDate("March 2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNotNull(sut_.execute(qo));

		dateA = dpm.parseDate("4 April 2000").iterator().next();
		dateB = dpm.parseDate("April 2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNull(sut_.execute(qo));

		dateA = dpm.parseDate("April 2000").iterator().next();
		dateB = dpm.parseDate("4 April 2000").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNull(sut_.execute(qo));

		dateA = dag_.findOrCreateNode("Today-Indexical", creator, true);
		dateB = dag_.findOrCreateNode("Yesterday-Indexical", creator, true);
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNotNull(sut_.execute(qo));

		dateA = dag_.findOrCreateNode("Today-Indexical", creator, true);
		dateB = dag_.findOrCreateNode("Tomorrow-Indexical", creator, true);
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNull(sut_.execute(qo));

		dateA = dag_.findOrCreateNode("Now", creator, true);
		dag_.findOrCreateNode("November", creator, true);
		dateB = dpm.parseDate("12 November 2013").iterator().next();
		qo = new QueryObject(laterThan, dateA, dateB);
		assertNotNull(sut_.execute(qo));
	}
}
