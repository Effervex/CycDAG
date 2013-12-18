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
import graph.core.CycDAGErrorEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
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
		Edge edge = sut_.findOrCreateEdge(creator, new Node[] { occupation,
				umaT, actor }, false);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));
		assertEquals(sut_.getNumEdges(), base + 0);

		// arg1isa
		Node argIsa = CommonConcepts.ARGISA.getNode(sut_);
		Node person = sut_.findOrCreateNode("Person", creator, true);
		Edge constraintEdge = sut_.findOrCreateEdge(creator, new Node[] {
				argIsa, occupation, PrimitiveNode.parseNode("1"), person },
				false);
		assertFalse(constraintEdge instanceof CycDAGErrorEdge);
		assertEquals(sut_.getNumEdges(), base + 1);

		edge = sut_.findOrCreateEdge(creator, new Node[] { occupation, umaT,
				actor }, false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);
		assertEquals(sut_.getNumEdges(), base + 1);

		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node collection = sut_.findOrCreateNode("Collection", creator, true);
		sut_.findOrCreateEdge(creator, new Node[] { isa, person, collection },
				false);
		assertEquals(sut_.getNumEdges(), base + 2);
		Edge isaPerson = sut_.findOrCreateEdge(creator, new Node[] { isa, umaT,
				person }, false);
		assertEdge(isaPerson);
		assertEquals(sut_.getNumEdges(), base + 3);
		edge = sut_.findOrCreateEdge(creator, new Node[] { occupation, umaT,
				actor }, false);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));
		assertEquals(sut_.getNumEdges(), base + 3);

		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation",
				creator, true);
		constraintEdge = sut_.findOrCreateEdge(creator, new Node[] { argIsa,
				occupation, PrimitiveNode.parseNode("2"), personType }, false);
		assertFalse(sut_.findOrCreateEdge(creator, new Node[] { isa,
				personType, collection }, false) instanceof CycDAGErrorEdge);
		assertFalse(sut_.findOrCreateEdge(creator, new Node[] { genls,
				personType, collection }, false) instanceof CycDAGErrorEdge);
		assertFalse(constraintEdge instanceof CycDAGErrorEdge);
		edge = sut_.findOrCreateEdge(creator, new Node[] { occupation, umaT,
				actor }, false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		Node entertainerType = sut_.findOrCreateNode(
				"EntertainerTypeByActivity", creator, true);
		assertFalse(sut_.findOrCreateEdge(creator, new Node[] { isa,
				entertainerType, collection }, false) instanceof CycDAGErrorEdge);
		assertFalse(sut_.findOrCreateEdge(creator, new Node[] { genls,
				entertainerType, personType }, false) instanceof CycDAGErrorEdge);
		assertFalse(sut_.findOrCreateEdge(creator, new Node[] { isa, actor,
				entertainerType }, false) instanceof CycDAGErrorEdge);
		edge = sut_.findOrCreateEdge(creator, new Node[] { occupation, umaT,
				actor }, false);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		Node argGenl = CommonConcepts.ARGGENL.getNode(sut_);
		constraintEdge = sut_.findOrCreateEdge(creator, new Node[] { argGenl,
				occupation, PrimitiveNode.parseNode("2"), person }, false);
		assertEdge(constraintEdge);
		edge = sut_.findOrCreateEdge(creator, new Node[] { occupation, umaT,
				actor }, false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { genls, actor,
				person }, false));
		edge = sut_.findOrCreateEdge(creator, new Node[] { occupation, umaT,
				actor }, false);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		// New node
		constraintEdge = sut_.findOrCreateEdge(creator, new Node[] { argIsa,
				isa, PrimitiveNode.parseNode("2"), collection }, false);
		Node sam = sut_.findOrCreateNode("SamSarjant", creator, true);
		Node mammal = sut_.findOrCreateNode("Mammal", creator, true);
		constraintEdge = sut_.findOrCreateEdge(creator, new Node[] { isa,
				mammal, collection }, false);
		assertFalse(constraintEdge instanceof CycDAGErrorEdge);
		edge = sut_.findOrCreateEdge(creator, new Node[] { isa, sam, mammal },
				false);
		assertFalse(edge instanceof CycDAGErrorEdge);

		constraintEdge = sut_.findOrCreateEdge(creator, new Node[] { argIsa,
				genls, PrimitiveNode.parseNode("1"), collection }, false);
		constraintEdge = sut_.findOrCreateEdge(creator, new Node[] { argIsa,
				genls, PrimitiveNode.parseNode("2"), collection }, false);
		edge = sut_.findOrCreateEdge(creator,
				new Node[] { genls, sam, mammal }, false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		// Adding text
		edge = sut_.findOrCreateEdge(creator, new Node[] { genls,
				new StringNode("cat"), mammal }, false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		edge = sut_.findOrCreateEdge(creator,
				new Node[] { genls, PrimitiveNode.parseNode("567"), mammal },
				false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);
	}

	@Test
	public void testSemanticArgCheckFunction() {
		Node creator = new StringNode("TestCreator");
		Node tastiness = sut_.findOrCreateNode("tastiness", creator, true);
		Node fruit = sut_.findOrCreateNode("Fruit", creator, true);
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), tastiness,
						PrimitiveNode.parseNode("1"), fruit }, false));
		Node fruitFn = sut_.findOrCreateNode("FruitFn", creator, true);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, fruitFn,
				CommonConcepts.FUNCTION.getNode(sut_) }, false));
		Node appleTree = sut_.findOrCreateNode("AppleTree", creator, true);
		Node plant = sut_.findOrCreateNode("Plant", creator, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, fruit,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, appleTree,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, plant,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1"), plant }, false));
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARITY.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1") }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { genls, fruit,
				plant }, false));
		Node fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", creator,
				true);
		assertNull(fruitFnNAT);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { genls,
				appleTree, plant }, false));
		fruitFnNAT = sut_
				.findOrCreateNode("(FruitFn AppleTree)", creator, true);
		assertNotNull(fruitFnNAT);

		Node yum = sut_.findOrCreateNode("Yum", creator, true);
		Edge edge = sut_.findOrCreateEdge(creator, new Node[] { tastiness,
				fruitFnNAT, yum }, false);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] {
				CommonConcepts.RESULT_GENL.getNode(sut_), fruitFn, fruit },
				false));
		edge = sut_.findOrCreateEdge(creator, new Node[] { tastiness,
				fruitFnNAT, yum }, false);
		assertEdge(edge);

		Node theFruit = sut_.findOrCreateNode("(TheFn Fruit)", creator, true);
		assertNotNull(theFruit);
		Node eat = sut_.findOrCreateNode("Eat", creator, true);
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), eat,
						PrimitiveNode.parseNode("1"), fruit }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { eat, theFruit },
				false));
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
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, researcher,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, person,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, personType,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), occupation,
						PrimitiveNode.parseNode("1"), person }, false));
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), occupation,
						PrimitiveNode.parseNode("2"), personType }, false));
		assertEdge(sut_.findOrCreateEdge(creator,
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), occupation,
						PrimitiveNode.parseNode("2"), person }, false));

		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { occupation, sam,
				researcher }, false, false, true));
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
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, dog,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, mammal,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { genls, dog,
				mammal }, false));
		Node fido = sut_.findOrCreateNode("Fido", creator, true);
		Edge edge = sut_.findOrCreateEdge(creator,
				new Node[] { isa, fido, dog }, false);
		assertEdge(edge);

		Node cat = sut_.findOrCreateNode("Cat", creator, true);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, cat,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { genls, cat,
				mammal }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { disjoint, cat,
				dog }, false));
		edge = sut_.findOrCreateEdge(creator, new Node[] { isa, fido, cat },
				false);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);

		Node donkey = sut_.findOrCreateNode("Donkey", creator, true);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, donkey,
				collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { genls, donkey,
				mammal }, false));
		Node species = sut_
				.findOrCreateNode("BiologicalSpecies", creator, true);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, species,
				collection }, false));
		Node siblingDisjoint = CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
				.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa,
				siblingDisjoint, collection }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, species,
				siblingDisjoint }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, dog,
				species }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, cat,
				species }, false));
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, donkey,
				species }, false));
		edge = sut_.findOrCreateEdge(creator, new Node[] { isa, fido, donkey },
				false);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);

		Node tomcat = sut_.findOrCreateNode("Tomcat", creator, true);
		assertEdge(sut_.findOrCreateEdge(creator, new Node[] { isa, tomcat,
				collection }, false));
		edge = sut_.findOrCreateEdge(creator,
				new Node[] { genls, tomcat, cat }, false);
		assertEdge(edge);
		edge = sut_.findOrCreateEdge(creator, new Node[] { isa, fido, tomcat },
				false);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);
		edge = sut_.findOrCreateEdge(creator,
				new Node[] { genls, tomcat, dog }, false);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);
		edge = sut_.findOrCreateEdge(creator,
				new Node[] { isa, tomcat, donkey }, false);
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
		Edge edge = sut_.findOrCreateEdge(creator, sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", creator, true, true), true);
		Edge otherEdge = sut_.findOrCreateEdge(creator, sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", creator, true, true), true);
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
		assertEquals(nodes.length, 2);
		assertEquals(nodes[0], CommonConcepts.NOT.getNode(sut_));
		assertTrue(nodes[1] instanceof OntologyFunction);
	}
}
