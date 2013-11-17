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
import graph.core.Node;
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
		Node occupation = sut_.findOrCreateNode("occupation", creator, true,
				true, true);
		Node umaT = sut_.findOrCreateNode("UmaThurman", creator, true, true,
				true);
		Node actor = sut_.findOrCreateNode("Actor", creator, true, true, true);
		Edge edge = sut_.findOrCreateEdge(creator, false, occupation, umaT,
				actor);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));
		assertEquals(sut_.getNumEdges(), base + 0);

		// arg1isa
		Node argIsa = CommonConcepts.ARGISA.getNode(sut_);
		Node person = sut_
				.findOrCreateNode("Person", creator, true, true, true);
		Edge constraintEdge = sut_.findOrCreateEdge(creator, false, argIsa,
				occupation, PrimitiveNode.parseNode("1"), person);
		assertFalse(constraintEdge instanceof CycDAGErrorEdge);
		assertEquals(sut_.getNumEdges(), base + 1);

		edge = sut_.findOrCreateEdge(creator, false, occupation, umaT, actor);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);
		assertEquals(sut_.getNumEdges(), base + 1);

		Node isa = CommonConcepts.ISA.getNode(sut_);
		Edge isaPerson = sut_.findOrCreateEdge(creator, false, isa, umaT,
				person);
		assertNotNull(isaPerson);
		assertEquals(sut_.getNumEdges(), base + 2);
		edge = sut_.findOrCreateEdge(creator, false, occupation, umaT, actor);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));
		assertEquals(sut_.getNumEdges(), base + 2);

		Node personType = sut_.findOrCreateNode("PersonTypeByOccupation",
				creator, true, true, true);
		constraintEdge = sut_.findOrCreateEdge(creator, false, argIsa,
				occupation, PrimitiveNode.parseNode("2"), personType);
		assertFalse(constraintEdge instanceof CycDAGErrorEdge);
		edge = sut_.findOrCreateEdge(creator, false, occupation, umaT, actor);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		Node entertainerType = sut_.findOrCreateNode(
				"EntertainerTypeByActivity", creator, true, true, true);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		assertNotNull(sut_.findOrCreateEdge(creator, false, isa, actor,
				entertainerType));
		assertEquals(
				sut_.findOrCreateEdge(creator, false, occupation, umaT, actor),
				CycDAGErrorEdge.SEMANTIC_CONFLICT);
		assertNotNull(sut_.findOrCreateEdge(creator, false, genls,
				entertainerType, personType));
		edge = sut_.findOrCreateEdge(creator, false, occupation, umaT, actor);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		Node argGenl = CommonConcepts.ARGGENL.getNode(sut_);
		constraintEdge = sut_.findOrCreateEdge(creator, false, argGenl,
				occupation, PrimitiveNode.parseNode("2"), person);
		assertNotNull(constraintEdge);
		edge = sut_.findOrCreateEdge(creator, false, occupation, umaT, actor);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		assertNotNull(sut_.findOrCreateEdge(creator, false, genls, actor,
				person));
		edge = sut_.findOrCreateEdge(creator, false, occupation, umaT, actor);
		assertFalse(edge instanceof CycDAGErrorEdge);
		assertTrue(sut_.removeEdge(edge));

		// New node
		DAGNode collection = CommonConcepts.COLLECTION.getNode(sut_);
		constraintEdge = sut_.findOrCreateEdge(creator, false, argIsa, isa,
				PrimitiveNode.parseNode("2"), collection);
		Node sam = sut_.findOrCreateNode("SamSarjant", creator, true, true,
				true);
		Node mammal = sut_
				.findOrCreateNode("Mammal", creator, true, true, true);
		constraintEdge = sut_.findOrCreateEdge(creator, false, isa, mammal,
				collection);
		assertFalse(constraintEdge instanceof CycDAGErrorEdge);
		edge = sut_.findOrCreateEdge(creator, false, isa, sam, mammal);
		assertFalse(edge instanceof CycDAGErrorEdge);

		constraintEdge = sut_.findOrCreateEdge(creator, false, argIsa, genls,
				PrimitiveNode.parseNode("1"), collection);
		constraintEdge = sut_.findOrCreateEdge(creator, false, argIsa, genls,
				PrimitiveNode.parseNode("2"), collection);
		edge = sut_.findOrCreateEdge(creator, false, genls, sam, mammal);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		// Adding text
		edge = sut_.findOrCreateEdge(creator, false, genls, new StringNode(
				"cat"), mammal);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);

		edge = sut_.findOrCreateEdge(creator, false, genls, new PrimitiveNode(
				567), mammal);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);
	}

	@Test
	public void testSemanticArgCheckFunction() {
		Node creator = new StringNode("TestCreator");
		Node tastiness = sut_.findOrCreateNode("tastiness", creator, true,
				true, true);
		Node fruit = sut_.findOrCreateNode("Fruit", creator, true, true, true);
		assertNotNull(sut_.findOrCreateEdge(creator, false,
				CommonConcepts.ARGGENL.getNode(sut_), tastiness,
				PrimitiveNode.parseNode("1"), fruit));
		Node fruitFn = sut_.findOrCreateNode("FruitFn", creator, true, true,
				true);
		Node appleTree = sut_.findOrCreateNode("AppleTree", creator, true,
				true, true);
		Node plant = sut_.findOrCreateNode("Plant", creator, true, true, true);
		assertNotNull(sut_.findOrCreateEdge(creator, false,
				CommonConcepts.ARGGENL.getNode(sut_), fruitFn,
				PrimitiveNode.parseNode("1"), plant));
		assertNotNull(sut_.findOrCreateEdge(creator, false,
				CommonConcepts.ARITY.getNode(sut_), fruitFn,
				PrimitiveNode.parseNode("1")));
		assertNotNull(sut_.findOrCreateEdge(creator, false,
				CommonConcepts.GENLS.getNode(sut_), fruit, plant));
		Node fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", creator,
				true, true, true);
		assertNull(fruitFnNAT);
		assertNotNull(sut_.findOrCreateEdge(creator, false,
				CommonConcepts.GENLS.getNode(sut_), appleTree, plant));
		fruitFnNAT = sut_.findOrCreateNode("(FruitFn AppleTree)", creator,
				true, true, true);
		assertNotNull(fruitFnNAT);

		Node yum = sut_.findOrCreateNode("Yum", creator, true, true, true);
		Edge edge = sut_.findOrCreateEdge(creator, false, tastiness,
				fruitFnNAT, yum);
		assertEquals(edge, CycDAGErrorEdge.SEMANTIC_CONFLICT);
		assertNotNull(sut_.findOrCreateEdge(creator, false,
				CommonConcepts.RESULTGENL.getNode(sut_), fruitFn, fruit));
		edge = sut_
				.findOrCreateEdge(creator, false, tastiness, fruitFnNAT, yum);
		assertNotNull(edge);
	}

	@Test
	public void testDisjointEdges() {
		Node creator = new StringNode("TestCreator");
		Node disjoint = CommonConcepts.DISJOINTWITH.getNode(sut_);
		Node isa = CommonConcepts.ISA.getNode(sut_);
		Node genls = CommonConcepts.GENLS.getNode(sut_);
		Node dog = sut_.findOrCreateNode("Dog", creator, true, true, true);
		Node mammal = sut_
				.findOrCreateNode("Mammal", creator, true, true, true);
		sut_.findOrCreateEdge(creator, false, genls, dog, mammal);
		Node fido = sut_.findOrCreateNode("Fido", creator, true, true, true);
		Edge edge = sut_.findOrCreateEdge(creator, false, isa, fido, dog);
		assertNotNull(edge);

		Node cat = sut_.findOrCreateNode("Cat", creator, true, true, true);
		sut_.findOrCreateEdge(creator, false, genls, cat, mammal);
		sut_.findOrCreateEdge(creator, false, disjoint, cat, dog);
		edge = sut_.findOrCreateEdge(creator, false, isa, fido, cat);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);

		Node donkey = sut_
				.findOrCreateNode("Donkey", creator, true, true, true);
		sut_.findOrCreateEdge(creator, false, genls, donkey, mammal);
		Node species = sut_.findOrCreateNode("BiologicalSpecies", creator,
				true, true, true);
		Node siblingDisjoint = CommonConcepts.SIBLING_DISJOINT_COLLECTION_TYPE
				.getNode(sut_);
		sut_.findOrCreateEdge(creator, false, isa, species, siblingDisjoint);
		sut_.findOrCreateEdge(creator, false, isa, dog, species);
		sut_.findOrCreateEdge(creator, false, isa, cat, species);
		sut_.findOrCreateEdge(creator, false, isa, donkey, species);
		edge = sut_.findOrCreateEdge(creator, false, isa, fido, donkey);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);

		Node tomcat = sut_
				.findOrCreateNode("Tomcat", creator, true, true, true);
		edge = sut_.findOrCreateEdge(creator, false, genls, tomcat, cat);
		assertNotNull(edge);
		edge = sut_.findOrCreateEdge(creator, false, isa, fido, tomcat);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);
		edge = sut_.findOrCreateEdge(creator, false, genls, tomcat, dog);
		assertEquals(edge, CycDAGErrorEdge.DISJOINT_EDGE);
		edge = sut_.findOrCreateEdge(creator, false, isa, tomcat, donkey);
		assertNotNull(edge);
		sut_.removeEdge(edge);
	}

	@Test
	public void testAddFunction() {
		sut_.noChecks_ = true;
		Node creator = new StringNode("TestCreator");
		Edge edge = sut_.findOrCreateEdge(creator, true, sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", creator, true, true));
		Edge otherEdge = sut_.findOrCreateEdge(creator, true, sut_.parseNodes(
				"(genls (SomeFn Blah) Person)", creator, true, true));
		for (int i = 0; i < edge.getNodes().length; i++) {
			DAGNode n1 = (DAGNode) edge.getNodes()[i];
			DAGNode n2 = (DAGNode) otherEdge.getNodes()[i];
			assertEquals(n1, n2);
			assertSame(n1, n2);
		}
	}
}
