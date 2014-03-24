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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DisjointErrorEdge;
import graph.core.ErrorEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
import graph.core.SemanticArgErrorEdge;
import graph.core.StringNode;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class CycDAGTest {
	private CycDAG sut_;

	/**
	 * 
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		sut_ = new CycDAG(new File("test"));
		CommonConcepts.initialise(sut_);
		CommonConcepts.createCommonAssertions(sut_);
	}

	@After
	public void tearDown() {
		sut_.clear();
	}

	@Test
	public void testSemanticArgCheck() {
		// No restrictions
		int base = sut_.getNumEdges();
		Node creator = new StringNode("TestCreator");
		Node occupation = sut_.findOrCreateNode("occupation", creator, true);
		Node umaT = sut_.findOrCreateNode("UmaThurman", creator, true);
		Node actor = sut_.findOrCreateNode("Actor", creator, true);
		Edge edge = sut_.findOrCreateEdge(
				new Node[] { occupation, umaT, actor }, creator, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));
		assertEquals(sut_.getNumEdges(), base + 0);

		// arg1isa
		Node argIsa = CommonConcepts.ARGISA.getNode(sut_);
		Node person = sut_.findOrCreateNode("Person", creator, true);
		Edge constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa,
				occupation, PrimitiveNode.parseNode("1"), person }, creator,
				true);
		assertFalse(constraintEdge instanceof ErrorEdge);
		assertEquals(sut_.getNumEdges(), base + 1);

		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);
		assertEquals(sut_.getNumEdges(), base + 1);

		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node collection = sut_.findOrCreateNode("Collection", creator, true);
		sut_.findOrCreateEdge(new Node[] { isa, person, collection }, creator,
				true);
		assertEquals(sut_.getNumEdges(), base + 2);
		Edge isaPerson = sut_.findOrCreateEdge(
				new Node[] { isa, umaT, person }, creator, true);
		assertEdge(isaPerson);
		assertEquals(sut_.getNumEdges(), base + 3);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));
		assertEquals(sut_.getNumEdges(), base + 3);

		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation",
				creator, true);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, occupation,
				PrimitiveNode.parseNode("2"), personType }, creator, true);
		assertFalse(sut_.findOrCreateEdge(new Node[] { isa, personType,
				collection }, creator, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { genls, personType,
				collection }, creator, true) instanceof ErrorEdge);
		assertFalse(constraintEdge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		Node entertainerType = sut_.findOrCreateNode(
				"EntertainerTypeByActivity", creator, true);
		assertFalse(sut_.findOrCreateEdge(new Node[] { isa, entertainerType,
				collection }, creator, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { genls, entertainerType,
				personType }, creator, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { isa, actor,
				entertainerType }, creator, true) instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		Node argGenl = CommonConcepts.ARGGENL.getNode(sut_);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argGenl,
				occupation, PrimitiveNode.parseNode("2"), person }, creator,
				true);
		assertEdge(constraintEdge);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, actor, person },
				creator, true));
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		// New node
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, isa,
				PrimitiveNode.parseNode("2"), collection }, creator, true);
		Node sam = sut_.findOrCreateNode("SamSarjant", creator, true);
		Node mammal = sut_.findOrCreateNode("Mammal", creator, true);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { isa, mammal,
				collection }, creator, true);
		assertFalse(constraintEdge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { isa, sam, mammal }, creator,
				true);
		assertFalse(edge instanceof ErrorEdge);

		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, genls,
				PrimitiveNode.parseNode("1"), collection }, creator, true);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, genls,
				PrimitiveNode.parseNode("2"), collection }, creator, true);
		edge = sut_.findOrCreateEdge(new Node[] { genls, sam, mammal },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		// Adding text
		edge = sut_.findOrCreateEdge(new Node[] { genls, new StringNode("cat"),
				mammal }, creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		edge = sut_.findOrCreateEdge(
				new Node[] { genls, PrimitiveNode.parseNode("567"), mammal },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);
	}

	@Test
	public void testSemanticArgCheckFunction() {
		Node creator = new StringNode("TestCreator");
		Node tastiness = sut_.findOrCreateNode("tastiness", creator, true);
		Node fruit = sut_.findOrCreateNode("Fruit", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), tastiness,
						PrimitiveNode.parseNode("1"), fruit }, creator, true));
		Node fruitFn = sut_.findOrCreateNode("FruitFn", creator, true);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, fruitFn,
				CommonConcepts.FUNCTION.getNode(sut_) }, creator, true));
		Node appleTree = sut_.findOrCreateNode("AppleTree", creator, true);
		Node plant = sut_.findOrCreateNode("Plant", creator, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, fruit, collection },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, appleTree,
				collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, plant, collection },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1"), plant }, creator, true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARITY.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1") }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, fruit, plant },
				creator, true));
		Node fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", creator,
				true);
		assertNull(fruitFnNAT);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { genls, appleTree, plant }, creator, true));
		fruitFnNAT = sut_
				.findOrCreateNode("(FruitFn AppleTree)", creator, true);
		assertNotNull(fruitFnNAT);

		Node yum = sut_.findOrCreateNode("Yum", creator, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { tastiness, fruitFnNAT,
				yum }, creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);
		assertEdge(sut_.findOrCreateEdge(new Node[] {
				CommonConcepts.RESULT_GENL.getNode(sut_), fruitFn, fruit },
				creator, true));
		edge = sut_.findOrCreateEdge(new Node[] { tastiness, fruitFnNAT, yum },
				creator, true);
		assertEdge(edge);

		Node theFruit = sut_.findOrCreateNode("(TheFn Fruit)", creator, true);
		assertNotNull(theFruit);
		Node eat = sut_.findOrCreateNode("Eat", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), eat,
						PrimitiveNode.parseNode("1"), fruit }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { eat, theFruit }, creator,
				true));
	}

	@Test
	public void testSemanticEdgeForce() {
		Node creator = new StringNode("TestCreator");
		Node occupation = sut_.findOrCreateNode("occupation", creator, true);
		Node sam = sut_.findOrCreateNode("Sam", creator, true);
		Node researcher = sut_.findOrCreateNode("Researcher", creator, true);
		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation",
				creator, true);
		Node person = sut_.findOrCreateNode("Person", creator, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, researcher,
				collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, person, collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, personType,
				collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), occupation,
						PrimitiveNode.parseNode("1"), person }, creator, true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), occupation,
						PrimitiveNode.parseNode("2"), personType }, creator,
				true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), occupation,
						PrimitiveNode.parseNode("2"), person }, creator, true));

		assertEdge(sut_.findOrCreateEdge(new Node[] { occupation, sam,
				researcher }, creator, true, false, true));
		assertNotNull(sut_.findEdge(isa, sam, person));
		assertNotNull(sut_.findEdge(isa, researcher, personType));
		assertNotNull(sut_.findEdge(genls, researcher, person));
	}

	@Test
	public void testDisjointEdges() {
		Node creator = new StringNode("TestCreator");
		Node disjoint = CommonConcepts.DISJOINTWITH.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", creator, true);
		Node mammal = sut_.findOrCreateNode("Mammal", creator, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, collection },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, mammal, collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, dog, mammal },
				creator, true));
		Node fido = sut_.findOrCreateNode("Fido", creator, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				creator, true);
		assertEdge(edge);

		Node cat = sut_.findOrCreateNode("Cat", creator, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, cat, collection },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, cat, mammal },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { disjoint, cat, dog },
				creator, true));
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, cat }, creator,
				true);
		assertTrue(edge instanceof DisjointErrorEdge);

		Node donkey = sut_.findOrCreateNode("Donkey", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, donkey, collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, donkey, mammal },
				creator, true));
		Node species = sut_
				.findOrCreateNode("BiologicalSpecies", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, species, collection }, creator, true));
		Node siblingDisjoint = CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
				.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, siblingDisjoint,
				collection }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, species,
				siblingDisjoint }, creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, species },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, cat, species },
				creator, true));
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, donkey, species },
				creator, true));
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, donkey }, creator,
				true);
		assertTrue(edge instanceof DisjointErrorEdge);

		Node tomcat = sut_.findOrCreateNode("Tomcat", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, tomcat, collection }, creator, true));
		edge = sut_.findOrCreateEdge(new Node[] { genls, tomcat, cat },
				creator, true);
		assertEdge(edge);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, tomcat }, creator,
				true);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls, tomcat, dog },
				creator, true);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { isa, tomcat, donkey },
				creator, true);
		assertEdge(edge);
		sut_.removeEdge(edge);
	}

	private void assertEdge(Edge edge) {
		assertFalse(edge.toString(), edge instanceof ErrorEdge);
	}

	@Test
	public void testAddFunction() {
		sut_.noChecks_ = true;
		Node creator = new StringNode("TestCreator");
		Edge edge = sut_.findOrCreateEdge(sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", creator, true, true), creator,
				true);
		Edge otherEdge = sut_.findOrCreateEdge(sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", creator, true, true), creator,
				true);
		for (int i = 0; i < edge.getNodes().length; i++) {
			DAGNode n1 = (DAGNode) edge.getNodes()[i];
			DAGNode n2 = (DAGNode) otherEdge.getNodes()[i];
			assertEquals(n1, n2);
			assertSame(n1, n2);
		}
	}

	@Test
	public void testParseNodes() {
		sut_.noChecks_ = true;
		Node creator = new StringNode("TestCreator");
		Node dog = sut_.findOrCreateNode("Dog", creator, true);
		Node cat = sut_.findOrCreateNode("Cat", creator, true);
		Node[] nodes = sut_.parseNodes("(not (genls Dog Cat))", creator, false,
				false);
		// TODO Not what I want
		assertEquals(nodes.length, 2);
		assertEquals(nodes[0], CommonConcepts.NOT.getNode(sut_));
		assertTrue(nodes[1] instanceof OntologyFunction);
	}
}
