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
import graph.core.DAGNode;
import graph.core.DisjointErrorEdge;
import graph.core.Edge;
import graph.core.ErrorEdge;
import graph.core.Node;
import graph.core.PrimitiveNode;
import graph.core.SemanticArgErrorEdge;
import graph.core.StringNode;
import graph.inference.CommonQuery;

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
		sut_ = new CycDAG(new File("test"), null, null);
		CommonConcepts.initialise(sut_);
		CommonQuery.reset();
		CommonConcepts.createCommonAssertions(sut_);
	}

	@After
	public void tearDown() {
		sut_.clear();
	}

	@Test
	public void testSemanticArgCheck() {
		// No restrictions
		Node creator = new StringNode("TestCreator");
		Node occupation = sut_.findOrCreateNode("occupation", creator, true);
		Node umaT = sut_.findOrCreateNode("UmaThurman", creator, true);
		Node actor = sut_.findOrCreateNode("Actor", creator, true);
		Edge edge = sut_.findOrCreateEdge(
				new Node[] { occupation, umaT, actor }, creator, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		// arg1isa
		Node argIsa = CommonConcepts.ARGISA.getNode(sut_);
		Node person = sut_.findOrCreateNode("Person", creator, true);
		Edge constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa,
				occupation, PrimitiveNode.parseNode("1"), person }, creator,
				true);
		assertFalse(constraintEdge instanceof ErrorEdge);

		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node collection = sut_.findOrCreateNode("Collection", creator, true);
		sut_.findOrCreateEdge(new Node[] { isa, person, collection }, creator,
				true);
		Edge isaPerson = sut_.findOrCreateEdge(
				new Node[] { isa, umaT, person }, creator, true);
		assertEdge(isaPerson, true);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

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
		assertEdge(constraintEdge, true);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, actor, person },
				creator, true), true);
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
		assertTrue(edge instanceof ErrorEdge);

		edge = sut_.findOrCreateEdge(
				new Node[] { genls, PrimitiveNode.parseNode("567"), mammal },
				creator, true);
		assertTrue(edge instanceof ErrorEdge);

		// Genls to an individual
		Node researcher = sut_.findOrCreateNode("Researcher", creator, true);
		edge = sut_.findOrCreateEdge(
				new Node[] { isa, researcher, collection }, creator, true);
		assertFalse(edge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls, researcher, sam },
				creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		// Testing Primitive Nodes
		Node nonNegative = sut_.findOrCreateNode("NonNegativeInteger", creator,
				true);
		edge = sut_.findOrCreateEdge(
				new Node[] { isa, nonNegative, collection }, creator, true);
		assertFalse(edge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls,
				CommonConcepts.POSITIVE_INTEGER.getNode(sut_), nonNegative },
				creator, true);
		assertFalse(edge.toString(), edge instanceof ErrorEdge);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, argIsa,
				PrimitiveNode.parseNode("2"), nonNegative }, creator, true);
		assertFalse(constraintEdge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(
				new Node[] { argIsa, isa, PrimitiveNode.parseNode("2"),
						collection }, creator, true);
		assertFalse(edge instanceof ErrorEdge);
	}

	@Test
	public void testSemanticArgCheckFunction() {
		Node creator = new StringNode("TestCreator");
		Node tastiness = sut_.findOrCreateNode("tastiness", creator, true);
		Node fruit = sut_.findOrCreateNode("Fruit", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), tastiness,
						PrimitiveNode.parseNode("1"), fruit }, creator, true),
				true);
		Node fruitFn = sut_.findOrCreateNode("FruitFn", creator, true);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, fruitFn,
						CommonConcepts.FUNCTION.getNode(sut_) }, creator, true),
				true);
		Node appleTree = sut_.findOrCreateNode("AppleTree", creator, true);
		Node plant = sut_.findOrCreateNode("Plant", creator, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, fruit, collection },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, appleTree,
				collection }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, plant, collection },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1"), plant }, creator, true),
				true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARITY.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1") }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, fruit, plant },
				creator, true), true);
		Node fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", creator,
				true);
		assertNull(fruitFnNAT);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { genls, appleTree, plant }, creator, true), true);
		fruitFnNAT = sut_
				.findOrCreateNode("(FruitFn AppleTree)", creator, true);
		assertNotNull(fruitFnNAT);

		Node yum = sut_.findOrCreateNode("Yum", creator, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { tastiness, fruitFnNAT,
				yum }, creator, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.RESULT_GENL.getNode(sut_),
								fruitFn, fruit }, creator, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { tastiness, fruitFnNAT, yum },
				creator, true);
		assertEdge(edge, true);

		Node theFruit = sut_.findOrCreateNode("(TheFn Fruit)", creator, true);
		assertNotNull(theFruit);
		Node eat = sut_.findOrCreateNode("Eat", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), eat,
						PrimitiveNode.parseNode("1"), fruit }, creator, true),
				true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { eat, theFruit }, creator,
				true), true);
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
				collection }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, person, collection }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, personType,
				collection }, creator, true), true);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGISA.getNode(sut_),
								occupation, PrimitiveNode.parseNode("1"),
								person }, creator, true), true);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGISA.getNode(sut_),
								occupation, PrimitiveNode.parseNode("2"),
								personType }, creator, true), true);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGGENL.getNode(sut_),
								occupation, PrimitiveNode.parseNode("2"),
								person }, creator, true), true);

		assertEdge(sut_.findOrCreateEdge(new Node[] { occupation, sam,
				researcher }, creator, true, false, true), true);
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
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, mammal, collection }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, dog, mammal },
				creator, true), true);
		Node fido = sut_.findOrCreateNode("Fido", creator, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				creator, true);
		assertEdge(edge, true);

		Node cat = sut_.findOrCreateNode("Cat", creator, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, cat, collection },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, cat, mammal },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { disjoint, cat, dog },
				creator, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, cat }, creator,
				true);
		assertTrue(edge instanceof ErrorEdge);

		Node donkey = sut_.findOrCreateNode("Donkey", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, donkey, collection }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, donkey, mammal },
				creator, true), true);
		Node species = sut_
				.findOrCreateNode("BiologicalSpecies", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, species, collection }, creator, true), true);
		Node siblingDisjoint = CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
				.getNode(sut_);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, siblingDisjoint,
						collection }, creator, true), true);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, species,
						siblingDisjoint }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, species },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, cat, species },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, donkey, species },
				creator, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, donkey }, creator,
				true);
		assertTrue(edge instanceof ErrorEdge);

		Node tomcat = sut_.findOrCreateNode("Tomcat", creator, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, tomcat, collection }, creator, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { genls, tomcat, cat },
				creator, true);
		assertEdge(edge, true);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, tomcat }, creator,
				true);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls, tomcat, dog },
				creator, true);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { isa, tomcat, donkey },
				creator, true);
		assertEdge(edge, true);
		sut_.removeEdge(edge);
	}

	@Test
	public void testInvalidDisjointEdge() {
		Node creator = new StringNode("TestCreator");
		Node disjoint = CommonConcepts.DISJOINTWITH.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", creator, true);
		Node mammal = sut_.findOrCreateNode("Mammal", creator, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, collection },
				creator, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, mammal, collection }, creator, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, dog, mammal },
				creator, true), true);
		Node fido = sut_.findOrCreateNode("Fido", creator, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				creator, true);
		assertEdge(edge, true);

		// Adding a disjoint edge to a known transitive connection.
		assertTrue(sut_.findOrCreateEdge(new Node[] { disjoint, dog, mammal },
				creator, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { disjoint, dog,
				collection }, creator, true) instanceof ErrorEdge);
		assertTrue(sut_.findOrCreateEdge(new Node[] { disjoint, dog, fido },
				creator, true) instanceof ErrorEdge);
	}

	@Test
	public void testCollectionOrders() {
		Node creator = new StringNode("TestCreator");
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node foc = CommonConcepts.FIRST_ORDER_COLLECTION.getNode(sut_);
		Node soc = CommonConcepts.SECOND_ORDER_COLLECTION.getNode(sut_);
		Node indiv = CommonConcepts.INDIVIDUAL.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", creator, true);
		Node fido = sut_.findOrCreateNode("Fido", creator, true);
		Node species = sut_
				.findOrCreateNode("BiologicalSpecies", creator, true);

		// Check Fido can be a dog
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, foc }, creator,
				true), true);
		Edge e = sut_.findOrCreateEdge(new Node[] { isa, fido, dog }, creator,
				true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { genls, fido, dog }, creator,
				true);
		// Cannot be true, as Fido is not a collection
		assertEdge(e, false);
		sut_.removeEdge(e);

		// Check Fido the individual can be a dog
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, fido, indiv },
				creator, true), true);
		e = sut_.findOrCreateEdge(new Node[] { isa, fido, dog }, creator, true);
		assertEdge(e, true);
		sut_.removeEdge(e);

		// Check Foxy the collection cannot be a dog
		Node foxy = sut_.findOrCreateNode("Foxy", creator, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, foxy, collection },
				creator, true), true);
		e = sut_.findOrCreateEdge(new Node[] { isa, foxy, dog }, creator, true);
		assertEdge(e, false);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { genls, foxy, dog }, creator,
				true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, foxy, foc },
				creator, true), true);
		e = sut_.findOrCreateEdge(new Node[] { genls, foxy, dog }, creator,
				true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		
		// Check Dog can be an instance of Biological Species
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, species, collection },
				creator, true), true);
		// This is fine while species is an underspecified collection
		e = sut_.findOrCreateEdge(new Node[] { isa, dog, species }, creator, true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, species, soc },
				creator, true), true);
		e = sut_.findOrCreateEdge(new Node[] { isa, dog, species }, creator, true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { genls, dog, species }, creator, true);
		assertEdge(e, false);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { isa, fido, species }, creator, true);
		assertEdge(e, false);
		sut_.removeEdge(e);
	}

	private void assertEdge(Edge edge, boolean asDAGEdge) {
		assertEquals(edge.toString(), !asDAGEdge, edge instanceof ErrorEdge);
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
	public void testProcessEdge() {
		Node creator = new StringNode("TestCreator");
		String edgeStr = "(#$genls #$FishingWithANet (#$AttemptingFn #$CatchingFish))	#$BaseKB";
		assertTrue(sut_.processEdge(edgeStr, creator));

		// NIL strings
		edgeStr = "(#$comment (NIL ((#$genls #$SpatialThingTypeByDimensionality #$SpatialThingTypeByShape))) \"dimensionality is orthogonal to shape. E.g., something that is an instance of #$NotVeryRoundObject might be 1-Dimentional, 2-Dimensional, or 3-Dimensional. \")       #$UniversalVocabularyMt";
		assertFalse(sut_.processEdge(edgeStr, creator));
		edgeStr = "(#$except (NIL ((#$negationInverse #$parts #$orgPresentInRegion))))     #$DualistGeopoliticalVocabularyMt";
		assertFalse(sut_.processEdge(edgeStr, creator));
	}
}
