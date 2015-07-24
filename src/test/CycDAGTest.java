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
		Edge edge = createEdge("(occupation UmaThurman Actor)", true, false, sut_);
		assertTrue(sut_.removeEdge(edge));

		createEdge("(argIsa occupation '1 Person)", true, false, sut_);

		edge = createEdge("(occupation UmaThurman Actor)", false, false, sut_);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		createEdge("(isa Person Collection)", true, false, sut_);
		createEdge("(isa UmaThurman Person)", true, false, sut_);
		edge = createEdge("(occupation UmaThurman Actor)", true, false, sut_);
		assertTrue(sut_.removeEdge(edge));

		createEdge("(argIsa occupation '2 PersonTypeByOccupation)", true, false, sut_);
		createEdge("(isa PersonTypeByOccupation Collection)", true, false, sut_);
		createEdge("(genls PersonTypeByOccupation Collection)", true, false, sut_);
		edge = createEdge("(occupation UmaThurman Actor)", false, false, sut_);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		edge = createEdge("(isa EntertainerTypeByActivity Collection)", true, false, sut_);
		edge = createEdge("(genls EntertainerTypeByActivity PersonTypeByOccupation)", true, false, sut_);
		edge = createEdge("(isa Actor EntertainerTypeByActivity)", true, false, sut_);
		edge = createEdge("(occupation UmaThurman Actor)", true, false, sut_);
		assertTrue(sut_.removeEdge(edge));

		createEdge("(argGenl occupation '2 Person)", true, true, sut_);
		edge = createEdge("(occupation UmaThurman Actor)", false, false, sut_);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		edge = createEdge("(genls Actor Person)", true, false, sut_);
		edge = createEdge("(occupation UmaThurman Actor)", true, false, sut_);
		assertTrue(sut_.removeEdge(edge));

		createEdge("(argIsa occupation '2 Collection)", true, false, sut_);
		edge = createEdge("(isa Mammal Collection)", true, false, sut_);
		edge = createEdge("(isa SamSarjant Mammal)", true, false, sut_);

		createEdge("(argIsa genls '1 Collection)", true, false, sut_);
		createEdge("(argIsa genls '2 Collection)", true, false, sut_);
		edge = createEdge("(genls SamSarjant Mammal)", false, false, sut_);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		// Adding text
		edge = createEdge("(genls \"Cat\" Mammal)", false, false, sut_);
		assertTrue(edge instanceof ErrorEdge);

		edge = createEdge("(genls '587 Mammal)", false, false, sut_);
		assertTrue(edge instanceof ErrorEdge);

		edge = createEdge("(isa Researcher Collection)", true, false, sut_);
		edge = createEdge("(genls Researcher SamSarjant)", false, false, sut_);
		assertTrue(edge instanceof SemanticArgErrorEdge);

		edge = createEdge("(isa NonNegativeInteger Collection)", true, false, sut_);
		edge = createEdge("(genls PositiveInteger NonNegativeInteger)", true, false, sut_);
		createEdge("(argIsa argIsa '2 NonNegativeInteger)", true, false, sut_);
		createEdge("(argIsa isa '2 Collection)", true, false, sut_);
	}

	@Test
	public void testSemanticArgCheckFunction() {
		createEdge("(argGenl tastiness '1 Fruit)", true, false, sut_);
		createEdge("(isa FruitFn Function-Denotational)", true, false, sut_);
		createEdge("(isa Fruit Collection)", true, false, sut_);
		createEdge("(isa AppleTree Collection)", true, false, sut_);
		createEdge("(isa Plant Collection)", true, false, sut_);
		createEdge("(argGenl FruitFn '1 Plant)", true, false, sut_);
		createEdge("(arity FruitFn '1)", true, false, sut_);
		createEdge("(genls Fruit Plant)", true, false, sut_);
		Node fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", DEFAULT_CREATOR, true);
		assertNull(fruitFnNAT);
		createEdge("(genls AppleTree Plant)", true, false, sut_);
		fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", DEFAULT_CREATOR, true);
		assertNotNull(fruitFnNAT);

		Edge edge = createEdge("(tastiness (FruitFn AppleTree) Yum)", false, false, sut_);
		assertTrue(edge instanceof SemanticArgErrorEdge);
		createEdge("(resultGenl FruitFn Fruit)", true, false, sut_);
		edge = createEdge("(tastiness (FruitFn AppleTree) Yum)", true, false, sut_);

		Node theFruit = sut_.findOrCreateNode("(TheFn Fruit)", DEFAULT_CREATOR, true);
		assertNotNull(theFruit);
		createEdge("(argIsa eat '1 Fruit)", true, false, sut_);
		createEdge("(eat (TheFn Fruit))", true, false, sut_);
	}

	@Test
	public void testSemanticEdgeForce() {
		Node sam = sut_.findOrCreateNode("Sam", DEFAULT_CREATOR, true);
		Node researcher = sut_.findOrCreateNode("Researcher", DEFAULT_CREATOR, true);
		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation", DEFAULT_CREATOR, true);
		Node person = sut_.findOrCreateNode("Person", DEFAULT_CREATOR, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		createEdge("(isa Researcher Collection)", true, false, sut_);
		createEdge("(isa Person Collection)", true, false, sut_);
		createEdge("(isa PersonTypeByOccupation Collection)", true, false, sut_);
		createEdge("(argIsa occupation '1 Person )", true, false, sut_);
		createEdge("(argIsa occupation '2 PersonTypeByOccupation )", true, false, sut_);
		createEdge("(argGenl occupation '2 Person )", true, false, sut_);

		createEdge("(occupation Sam Researcher)", true, true, sut_);
		assertNotNull(sut_.findEdge(isa, sam, person));
		assertNotNull(sut_.findEdge(isa, researcher, personType));
		assertNotNull(sut_.findEdge(genls, researcher, person));
	}

	@Test
	public void testDisjointEdges() {
		createEdge("(isa Dog Collection)", true, false, sut_);
		createEdge("(isa Mammal Collection)", true, false, sut_);
		createEdge("(genls Dog Mammal)", true, false, sut_);
		Edge edge = createEdge("(isa Fido Dog)", true, false, sut_);

		createEdge("(isa Cat Collection)", true, false, sut_);
		createEdge("(genls Cat Mammal)", true, false, sut_);
		createEdge("(disjointWith Cat Dog)", true, false, sut_);
		edge = createEdge("(isa Fido Cat)", false, false, sut_);
		assertTrue(edge instanceof ErrorEdge);

		createEdge("(isa Donkey Collection)", true, false, sut_);
		createEdge("(genls Donkey Mammal)", true, false, sut_);
		createEdge("(isa BiologicalSpecies Collection)", true, false, sut_);
		createEdge("(isa SiblingDisjointCollectionType Collection)", true, false, sut_);
		createEdge("(isa BiologicalSpecies SiblingDisjointCollectionType)", true, false, sut_);
		createEdge("(isa Dog BiologicalSpecies)", true, false, sut_);
		createEdge("(isa Cat BiologicalSpecies)", true, false, sut_);
		createEdge("(isa Donkey BiologicalSpecies)", true, false, sut_);
		edge = createEdge("(isa Fido Donkey)", false, false, sut_);
		assertTrue(edge instanceof ErrorEdge);

		createEdge("(isa Tomcat Collection)", true, false, sut_);
		edge = createEdge("(genls Tomcat Cat)", true, false, sut_);
		edge = createEdge("(isa Fido Tomcat)", false, false, sut_);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = createEdge("(genls Tomcat Dog)", false, false, sut_);
		assertTrue(edge instanceof DisjointErrorEdge);
		edge = createEdge("(isa Tomcat Donkey)", true, false, sut_);
		sut_.removeEdge(edge);
	}

	@Test
	public void testInvalidDisjointEdge() {
		createEdge("(isa Dog Collection)", true, false, sut_);
		createEdge("(isa Mammal Collection)", true, false, sut_);
		createEdge("(genls Dog Mammal)", true, false, sut_);
		createEdge("(isa Fido Dog)", true, false, sut_);

		// Adding a disjoint edge to a known transitive connection.
		assertTrue(createEdge("(disjointWith Dog Mammal)", false, false, sut_) instanceof ErrorEdge);
		assertFalse(createEdge("(disjointWith Dog Collection)", true, false, sut_) instanceof ErrorEdge);
		assertTrue(createEdge("(disjointWith Dog Fido)", false, false, sut_) instanceof ErrorEdge);
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
		Node species = sut_.findOrCreateNode("BiologicalSpecies", DEFAULT_CREATOR, true);

		// Check Fido can be a dog
		createEdge("(isa Dog FirstOrderCollection)", true, true, sut_);
		Edge e = createEdge("(isa Fido Dog)", true, true, sut_);
		sut_.removeEdge(e);
		e = createEdge("(genls Fido Dog)", false, false, sut_);
		// Cannot be true, as Fido is not a collection
		assertTrue(e instanceof ErrorEdge);
		sut_.removeEdge(e);

		// Check Fido the individual can be a dog
		createEdge("(isa Fido Individual)", true, false, sut_);
		e = createEdge("(isa Fido Dog)", true, false, sut_);
		sut_.removeEdge(e);

		// Check Foxy the collection cannot be a dog
		Node foxy = sut_.findOrCreateNode("Foxy", DEFAULT_CREATOR, true);
		createEdge("(isa Foxy Collection)", true, false, sut_);
		e = createEdge("(isa Foxy Dog)", false, false, sut_);
		assertTrue(e instanceof ErrorEdge);
		sut_.removeEdge(e);
		e = createEdge("(genls Foxy Dog)", true, false, sut_);
		sut_.removeEdge(e);
		createEdge("(isa Foxy FirstOrderCollection)", true, false, sut_);
		e = createEdge("(genls Foxy Dog)", true, false, sut_);
		sut_.removeEdge(e);

		// Check Dog can be an instance of Biological Species
		createEdge("(isa BiologicalSpecies Collection)", true, false, sut_);
		// This is fine while species is an underspecified collection
		e = createEdge("(isa Dog BiologicalSpecies)", true, false, sut_);
		sut_.removeEdge(e);
		createEdge("(isa BiologicalSpecies SecondOrderCollection)", true, false, sut_);
		e = createEdge("(isa Dog BiologicalSpecies)", true, false, sut_);
		sut_.removeEdge(e);
		e = createEdge("(genls Dog BiologicalSpecies)", false, false, sut_);
		assertTrue(e instanceof ErrorEdge);
		sut_.removeEdge(e);
		e = createEdge("(isa Fido BiologicalSpecies)", false, false, sut_);
		assertTrue(e instanceof ErrorEdge);
		sut_.removeEdge(e);
	}

	@Test
	public void testAddFunction() {
		sut_.noChecks_ = true;
		Edge edge = sut_.findOrCreateEdge(sut_.parseNodes("(genls (SomeFn Blah) Person)", DEFAULT_CREATOR, true, true), DEFAULT_CREATOR, true);
		Edge otherEdge = sut_.findOrCreateEdge(sut_.parseNodes("(genls (SomeFn Blah) Person)", DEFAULT_CREATOR, true, true), DEFAULT_CREATOR, true);
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
		createEdge("(isa TestX TestA)", true, true, sut_);
		createEdge("(isa TestX TestB)", true, true, sut_);
		createEdge("(genls TestA TestC)", true, true, sut_);
		createEdge("(genls TestB TestD)", true, true, sut_);

		// Check immediately contradictory disjoint
		Edge e = createEdge("(disjointWith TestC TestD)", false, true, sut_);
		assertTrue(e.toString(), e instanceof ErrorEdge);

		// Check isa disjoint rejection
		createEdge("(disjointWith TestB TestB2)", true, true, sut_);
		e = createEdge("(isa TestX TestB2)", false, true, sut_);
		assertTrue(e.toString(), e instanceof ErrorEdge);

		// Create separate disjoint, then join
		createEdge("(disjointWith TestE TestD)", true, true, sut_);
		// Join
		e = createEdge("(genls TestC TestE)", false, true, sut_);
		assertTrue(e.toString(), e instanceof ErrorEdge);
	}

	public static Edge createEdge(String edgeString, boolean assertExists, boolean forceConstraints, CycDAG dag) {
		Node[] nodes = dag.parseNodes(edgeString, DEFAULT_CREATOR, true, false);
		Edge edge = dag.findOrCreateEdge(nodes, DEFAULT_CREATOR, null, true, false, forceConstraints);
		if (assertExists)
			assertFalse(edge.toString(), edge instanceof ErrorEdge);
		return edge;
	}
}
