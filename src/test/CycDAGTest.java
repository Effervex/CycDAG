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
import graph.module.QueryModule;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class CycDAGTest {
	private static final Node DEFAULT_CREATOR = new StringNode("TestCreator");
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
		Node occupation = sut_.findOrCreateNode("occupation", DEFAULT_CREATOR,
				true);
		Node umaT = sut_.findOrCreateNode("UmaThurman", DEFAULT_CREATOR, true);
		Node actor = sut_.findOrCreateNode("Actor", DEFAULT_CREATOR, true);
		Edge edge = sut_.findOrCreateEdge(
				new Node[] { occupation, umaT, actor }, DEFAULT_CREATOR, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		// arg1isa
		Node argIsa = CommonConcepts.ARGISA.getNode(sut_);
		Node person = sut_.findOrCreateNode("Person", DEFAULT_CREATOR, true);
		Edge constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa,
				occupation, PrimitiveNode.parseNode("1"), person },
				DEFAULT_CREATOR, true);
		assertFalse(constraintEdge instanceof ErrorEdge);

		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node collection = sut_.findOrCreateNode("Collection", DEFAULT_CREATOR,
				true);
		sut_.findOrCreateEdge(new Node[] { isa, person, collection },
				DEFAULT_CREATOR, true);
		Edge isaPerson = sut_.findOrCreateEdge(
				new Node[] { isa, umaT, person }, DEFAULT_CREATOR, true);
		assertEdge(isaPerson, true);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				DEFAULT_CREATOR, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation",
				DEFAULT_CREATOR, true);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, occupation,
				PrimitiveNode.parseNode("2"), personType }, DEFAULT_CREATOR,
				true);
		assertFalse(sut_.findOrCreateEdge(new Node[] { isa, personType,
				collection }, DEFAULT_CREATOR, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { genls, personType,
				collection }, DEFAULT_CREATOR, true) instanceof ErrorEdge);
		assertFalse(constraintEdge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		Node entertainerType = sut_.findOrCreateNode(
				"EntertainerTypeByActivity", DEFAULT_CREATOR, true);
		assertFalse(sut_.findOrCreateEdge(new Node[] { isa, entertainerType,
				collection }, DEFAULT_CREATOR, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { genls, entertainerType,
				personType }, DEFAULT_CREATOR, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { isa, actor,
				entertainerType }, DEFAULT_CREATOR, true) instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				DEFAULT_CREATOR, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		Node argGenl = CommonConcepts.ARGGENL.getNode(sut_);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argGenl,
				occupation, PrimitiveNode.parseNode("2"), person },
				DEFAULT_CREATOR, true);
		assertEdge(constraintEdge, true);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, actor, person },
				DEFAULT_CREATOR, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { occupation, umaT, actor },
				DEFAULT_CREATOR, true);
		assertFalse(edge instanceof ErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		// New node
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, isa,
				PrimitiveNode.parseNode("2"), collection }, DEFAULT_CREATOR,
				true);
		Node sam = sut_.findOrCreateNode("SamSarjant", DEFAULT_CREATOR, true);
		Node mammal = sut_.findOrCreateNode("Mammal", DEFAULT_CREATOR, true);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { isa, mammal,
				collection }, DEFAULT_CREATOR, true);
		assertFalse(constraintEdge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { isa, sam, mammal },
				DEFAULT_CREATOR, true);
		assertFalse(edge instanceof ErrorEdge);

		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, genls,
				PrimitiveNode.parseNode("1"), collection }, DEFAULT_CREATOR,
				true);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, genls,
				PrimitiveNode.parseNode("2"), collection }, DEFAULT_CREATOR,
				true);
		edge = sut_.findOrCreateEdge(new Node[] { genls, sam, mammal },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		// Adding text
		edge = sut_.findOrCreateEdge(new Node[] { genls, new StringNode("cat"),
				mammal }, DEFAULT_CREATOR, true);
		assertTrue(edge instanceof ErrorEdge);

		edge = sut_.findOrCreateEdge(
				new Node[] { genls, PrimitiveNode.parseNode("567"), mammal },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof ErrorEdge);

		// Genls to an individual
		Node researcher = sut_.findOrCreateNode("Researcher", DEFAULT_CREATOR,
				true);
		edge = sut_.findOrCreateEdge(
				new Node[] { isa, researcher, collection }, DEFAULT_CREATOR,
				true);
		assertFalse(edge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls, researcher, sam },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		// Testing Primitive Nodes
		Node nonNegative = sut_.findOrCreateNode("NonNegativeInteger",
				DEFAULT_CREATOR, true);
		edge = sut_.findOrCreateEdge(
				new Node[] { isa, nonNegative, collection }, DEFAULT_CREATOR,
				true);
		assertFalse(edge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls,
				CommonConcepts.POSITIVE_INTEGER.getNode(sut_), nonNegative },
				DEFAULT_CREATOR, true);
		assertFalse(edge.toString(), edge instanceof ErrorEdge);
		constraintEdge = sut_.findOrCreateEdge(new Node[] { argIsa, argIsa,
				PrimitiveNode.parseNode("2"), nonNegative }, DEFAULT_CREATOR,
				true);
		assertFalse(constraintEdge instanceof ErrorEdge);
		edge = sut_.findOrCreateEdge(
				new Node[] { argIsa, isa, PrimitiveNode.parseNode("2"),
						collection }, DEFAULT_CREATOR, true);
		assertFalse(edge instanceof ErrorEdge);
	}

	@Test
	public void testSemanticArgCheckFunction() {
		Node tastiness = sut_.findOrCreateNode("tastiness", DEFAULT_CREATOR,
				true);
		Node fruit = sut_.findOrCreateNode("Fruit", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), tastiness,
						PrimitiveNode.parseNode("1"), fruit }, DEFAULT_CREATOR,
				true), true);
		Node fruitFn = sut_.findOrCreateNode("FruitFn", DEFAULT_CREATOR, true);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, fruitFn,
						CommonConcepts.FUNCTION.getNode(sut_) },
						DEFAULT_CREATOR, true), true);
		Node appleTree = sut_.findOrCreateNode("AppleTree", DEFAULT_CREATOR,
				true);
		Node plant = sut_.findOrCreateNode("Plant", DEFAULT_CREATOR, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, fruit, collection },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, appleTree,
				collection }, DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, plant, collection },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGGENL.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1"), plant }, DEFAULT_CREATOR,
				true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARITY.getNode(sut_), fruitFn,
						PrimitiveNode.parseNode("1") }, DEFAULT_CREATOR, true),
				true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, fruit, plant },
				DEFAULT_CREATOR, true), true);
		Node fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)",
				DEFAULT_CREATOR, true);
		assertNull(fruitFnNAT);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { genls, appleTree, plant }, DEFAULT_CREATOR, true),
				true);
		fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)",
				DEFAULT_CREATOR, true);
		assertNotNull(fruitFnNAT);

		Node yum = sut_.findOrCreateNode("Yum", DEFAULT_CREATOR, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { tastiness, fruitFnNAT,
				yum }, DEFAULT_CREATOR, true);
		assertTrue(edge instanceof SemanticArgErrorEdge);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.RESULT_GENL.getNode(sut_),
								fruitFn, fruit }, DEFAULT_CREATOR, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { tastiness, fruitFnNAT, yum },
				DEFAULT_CREATOR, true);
		assertEdge(edge, true);

		Node theFruit = sut_.findOrCreateNode("(TheFn Fruit)", DEFAULT_CREATOR,
				true);
		assertNotNull(theFruit);
		Node eat = sut_.findOrCreateNode("Eat", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(sut_), eat,
						PrimitiveNode.parseNode("1"), fruit }, DEFAULT_CREATOR,
				true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { eat, theFruit },
				DEFAULT_CREATOR, true), true);
	}

	@Test
	public void testSemanticEdgeForce() {
		Node occupation = sut_.findOrCreateNode("occupation", DEFAULT_CREATOR,
				true);
		Node sam = sut_.findOrCreateNode("Sam", DEFAULT_CREATOR, true);
		Node researcher = sut_.findOrCreateNode("Researcher", DEFAULT_CREATOR,
				true);
		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation",
				DEFAULT_CREATOR, true);
		Node person = sut_.findOrCreateNode("Person", DEFAULT_CREATOR, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, researcher,
				collection }, DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, person, collection }, DEFAULT_CREATOR, true),
				true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, personType,
				collection }, DEFAULT_CREATOR, true), true);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGISA.getNode(sut_),
								occupation, PrimitiveNode.parseNode("1"),
								person }, DEFAULT_CREATOR, true), true);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGISA.getNode(sut_),
								occupation, PrimitiveNode.parseNode("2"),
								personType }, DEFAULT_CREATOR, true), true);
		assertEdge(
				sut_.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGGENL.getNode(sut_),
								occupation, PrimitiveNode.parseNode("2"),
								person }, DEFAULT_CREATOR, true), true);

		assertEdge(sut_.findOrCreateEdge(new Node[] { occupation, sam,
				researcher }, DEFAULT_CREATOR, true, false, true), true);
		assertNotNull(sut_.findEdge(isa, sam, person));
		assertNotNull(sut_.findEdge(isa, researcher, personType));
		assertNotNull(sut_.findEdge(genls, researcher, person));
	}

	@Test
	public void testDisjointEdges() {
		Node disjoint = CommonConcepts.DISJOINTWITH.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", DEFAULT_CREATOR, true);
		Node mammal = sut_.findOrCreateNode("Mammal", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, collection },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, mammal, collection }, DEFAULT_CREATOR, true),
				true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, dog, mammal },
				DEFAULT_CREATOR, true), true);
		Node fido = sut_.findOrCreateNode("Fido", DEFAULT_CREATOR, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				DEFAULT_CREATOR, true);
		assertEdge(edge, true);

		Node cat = sut_.findOrCreateNode("Cat", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, cat, collection },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, cat, mammal },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { disjoint, cat, dog },
				DEFAULT_CREATOR, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, cat },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof ErrorEdge);

		Node donkey = sut_.findOrCreateNode("Donkey", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, donkey, collection }, DEFAULT_CREATOR, true),
				true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, donkey, mammal },
				DEFAULT_CREATOR, true), true);
		Node species = sut_.findOrCreateNode("BiologicalSpecies",
				DEFAULT_CREATOR, true);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, species, collection },
						DEFAULT_CREATOR, true), true);
		Node siblingDisjoint = CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
				.getNode(sut_);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, siblingDisjoint,
						collection }, DEFAULT_CREATOR, true), true);
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, species,
						siblingDisjoint }, DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, species },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, cat, species },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, donkey, species },
				DEFAULT_CREATOR, true), true);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, donkey },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof ErrorEdge);

		Node tomcat = sut_.findOrCreateNode("Tomcat", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, tomcat, collection }, DEFAULT_CREATOR, true),
				true);
		edge = sut_.findOrCreateEdge(new Node[] { genls, tomcat, cat },
				DEFAULT_CREATOR, true);
		assertEdge(edge, true);
		edge = sut_.findOrCreateEdge(new Node[] { isa, fido, tomcat },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { genls, tomcat, dog },
				DEFAULT_CREATOR, true);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = sut_.findOrCreateEdge(new Node[] { isa, tomcat, donkey },
				DEFAULT_CREATOR, true);
		assertEdge(edge, true);
		sut_.removeEdge(edge);
	}

	@Test
	public void testInvalidDisjointEdge() {
		Node disjoint = CommonConcepts.DISJOINTWITH.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", DEFAULT_CREATOR, true);
		Node mammal = sut_.findOrCreateNode("Mammal", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, collection },
				DEFAULT_CREATOR, true), true);
		assertEdge(sut_.findOrCreateEdge(
				new Node[] { isa, mammal, collection }, DEFAULT_CREATOR, true),
				true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { genls, dog, mammal },
				DEFAULT_CREATOR, true), true);
		Node fido = sut_.findOrCreateNode("Fido", DEFAULT_CREATOR, true);
		Edge edge = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				DEFAULT_CREATOR, true);
		assertEdge(edge, true);

		// Adding a disjoint edge to a known transitive connection.
		assertTrue(sut_.findOrCreateEdge(new Node[] { disjoint, dog, mammal },
				DEFAULT_CREATOR, true) instanceof ErrorEdge);
		assertFalse(sut_.findOrCreateEdge(new Node[] { disjoint, dog,
				collection }, DEFAULT_CREATOR, true) instanceof ErrorEdge);
		assertTrue(sut_.findOrCreateEdge(new Node[] { disjoint, dog, fido },
				DEFAULT_CREATOR, true) instanceof ErrorEdge);
	}

	@Test
	public void testCollectionOrders() {
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node foc = CommonConcepts.FIRST_ORDER_COLLECTION.getNode(sut_);
		Node soc = CommonConcepts.SECOND_ORDER_COLLECTION.getNode(sut_);
		Node indiv = CommonConcepts.INDIVIDUAL.getNode(sut_);
		Node collection = CommonConcepts.COLLECTION.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", DEFAULT_CREATOR, true);
		Node fido = sut_.findOrCreateNode("Fido", DEFAULT_CREATOR, true);
		Node species = sut_.findOrCreateNode("BiologicalSpecies",
				DEFAULT_CREATOR, true);

		// Check Fido can be a dog
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, dog, foc },
				DEFAULT_CREATOR, true), true);
		Edge e = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				DEFAULT_CREATOR, true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { genls, fido, dog },
				DEFAULT_CREATOR, true);
		// Cannot be true, as Fido is not a collection
		assertEdge(e, false);
		sut_.removeEdge(e);

		// Check Fido the individual can be a dog
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, fido, indiv },
				DEFAULT_CREATOR, true), true);
		e = sut_.findOrCreateEdge(new Node[] { isa, fido, dog },
				DEFAULT_CREATOR, true);
		assertEdge(e, true);
		sut_.removeEdge(e);

		// Check Foxy the collection cannot be a dog
		Node foxy = sut_.findOrCreateNode("Foxy", DEFAULT_CREATOR, true);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, foxy, collection },
				DEFAULT_CREATOR, true), true);
		e = sut_.findOrCreateEdge(new Node[] { isa, foxy, dog },
				DEFAULT_CREATOR, true);
		assertEdge(e, false);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { genls, foxy, dog },
				DEFAULT_CREATOR, true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, foxy, foc },
				DEFAULT_CREATOR, true), true);
		e = sut_.findOrCreateEdge(new Node[] { genls, foxy, dog },
				DEFAULT_CREATOR, true);
		assertEdge(e, true);
		sut_.removeEdge(e);

		// Check Dog can be an instance of Biological Species
		assertEdge(
				sut_.findOrCreateEdge(new Node[] { isa, species, collection },
						DEFAULT_CREATOR, true), true);
		// This is fine while species is an underspecified collection
		e = sut_.findOrCreateEdge(new Node[] { isa, dog, species },
				DEFAULT_CREATOR, true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		assertEdge(sut_.findOrCreateEdge(new Node[] { isa, species, soc },
				DEFAULT_CREATOR, true), true);
		e = sut_.findOrCreateEdge(new Node[] { isa, dog, species },
				DEFAULT_CREATOR, true);
		assertEdge(e, true);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { genls, dog, species },
				DEFAULT_CREATOR, true);
		assertEdge(e, false);
		sut_.removeEdge(e);
		e = sut_.findOrCreateEdge(new Node[] { isa, fido, species },
				DEFAULT_CREATOR, true);
		assertEdge(e, false);
		sut_.removeEdge(e);
	}

	private void assertEdge(Edge edge, boolean asDAGEdge) {
		assertEquals(edge.toString(), !asDAGEdge, edge instanceof ErrorEdge);
	}

	@Test
	public void testAddFunction() {
		sut_.noChecks_ = true;
		Edge edge = sut_.findOrCreateEdge(sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", DEFAULT_CREATOR, true, true),
				DEFAULT_CREATOR, true);
		Edge otherEdge = sut_.findOrCreateEdge(sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", DEFAULT_CREATOR, true, true),
				DEFAULT_CREATOR, true);
		for (int i = 0; i < edge.getNodes().length; i++) {
			DAGNode n1 = (DAGNode) edge.getNodes()[i];
			DAGNode n2 = (DAGNode) otherEdge.getNodes()[i];
			assertEquals(n1, n2);
			assertSame(n1, n2);
		}
	}

	@Test
	public void testProcessEdge() {
		String edgeStr = "(#$genls #$FishingWithANet (#$AttemptingFn #$CatchingFish))	#$BaseKB";
		assertTrue(sut_.processEdge(edgeStr, DEFAULT_CREATOR));

		// NIL strings
		edgeStr = "(#$comment (NIL ((#$genls #$SpatialThingTypeByDimensionality #$SpatialThingTypeByShape))) \"dimensionality is orthogonal to shape. E.g., something that is an instance of #$NotVeryRoundObject might be 1-Dimentional, 2-Dimensional, or 3-Dimensional. \")       #$UniversalVocabularyMt";
		assertFalse(sut_.processEdge(edgeStr, DEFAULT_CREATOR));
		edgeStr = "(#$except (NIL ((#$negationInverse #$parts #$orgPresentInRegion))))     #$DualistGeopoliticalVocabularyMt";
		assertFalse(sut_.processEdge(edgeStr, DEFAULT_CREATOR));
	}

	@Test
	public void testHighLevelContradictions() {
		// Set up basic network
		createEdge("(isa TestX TestA)", true, sut_);
		createEdge("(isa TestX TestB)", true, sut_);
		createEdge("(genls TestA TestC)", true, sut_);
		createEdge("(genls TestB TestD)", true, sut_);

		// Check immediately contradictory disjoint
		Edge e = createEdge("(disjointWith TestC TestD)", false, sut_);
		assertTrue(e.toString(), e instanceof ErrorEdge);

		// Check isa disjoint rejection
		createEdge("(disjointWith TestB TestB2)", true, sut_);
		e = createEdge("(isa TestX TestB2)", false, sut_);
		assertTrue(e.toString(), e instanceof ErrorEdge);

		// Create separate disjoint, then join
		createEdge("(disjointWith TestE TestD)", true, sut_);
		// Join
		e = createEdge("(genls TestC TestE)", false, sut_);
		assertTrue(e.toString(), e instanceof ErrorEdge);
	}

	public static Edge createEdge(String edgeString, boolean assertExists,
			CycDAG dag) {
		Node[] nodes = dag.parseNodes(edgeString, DEFAULT_CREATOR, true, false);
		Edge edge = dag.findOrCreateEdge(nodes, DEFAULT_CREATOR, null, true,
				false, true);
		if (assertExists)
			assertFalse(edge instanceof ErrorEdge);
		return edge;
	}
}
