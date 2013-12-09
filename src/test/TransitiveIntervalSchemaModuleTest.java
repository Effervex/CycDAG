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
		sut_ = (TransitiveIntervalSchemaModule) dag_
				.getModule(TransitiveIntervalSchemaModule.class);
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
		Node dog = dag_.findOrCreateNode("Dog", creator, true, true, false);
		Node cat = dag_.findOrCreateNode("Cat", creator, true, true, false);
		Node canis = dag_.findOrCreateNode("Canis", creator, true, true, false);
		Node thing = dag_.findOrCreateNode("Thing", creator, true, true, false);
		Node pet = dag_.findOrCreateNode("Pet", creator, true, true, false);
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

		Node dog = dag_.findOrCreateNode("Dog", creator, true, true, false);
		Node cat = dag_.findOrCreateNode("Cat", creator, true, true, false);
		Node canis = dag_.findOrCreateNode("Canis", creator, true, true, false);
		Node mammal = dag_.findOrCreateNode("Mammal", creator, true, true,
				false);
		Node thing = dag_.findOrCreateNode("Thing", creator, true, true, false);
		Node pet = dag_.findOrCreateNode("Pet", creator, true, true, false);
		sut_.initialisationComplete(dag_.getNodes(), dag_.getEdges());
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
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true,
				true, false);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true,
				true, false);
		DAGNode cat = (DAGNode) dag_.findOrCreateNode("Cat", creator, true,
				true, false);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true,
				true, false);
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator,
				true, true, false);
		DAGNode fluffy = (DAGNode) dag_.findOrCreateNode("Fluffy", creator,
				true, true, false);
		sut_.initialisationComplete(dag_.getNodes(), dag_.getEdges());

		// Existing nodes
		dag_.noChecks_ = true;
		assertFalse(dag_.findOrCreateEdge(creator, false, genls, canis, fluffy) instanceof ErrorEdge);
		assertNotNull(sut_.execute(true, canis, fluffy));
		assertNotNull(sut_.execute(true, dog, fluffy));
		assertTrue(sut_.execute(true, dog).contains(fluffy));
		assertFalse(sut_.execute(true, cat).contains(dog));
		assertFalse(sut_.execute(true, dog).contains(cat));
		assertFalse(sut_.execute(false, cat).contains(dog));
		assertFalse(sut_.execute(false, dog).contains(cat));
		assertTrue(sut_.execute(false, fluffy).contains(dog));

		// Tree addition (genls Mammal Animal)
		DAGNode animal = (DAGNode) dag_.findOrCreateNode("Animal", creator,
				true, true, false);
		assertFalse(dag_
				.findOrCreateEdge(creator, false, genls, mammal, animal) instanceof ErrorEdge);
		assertNotNull(sut_.execute(true, mammal, animal));
		assertNotNull(sut_.execute(true, dog, animal));
		assertNotNull(sut_.execute(true, cat, animal));

		// Non-tree addition (genls Cow Mammal)
		DAGNode cow = (DAGNode) dag_.findOrCreateNode("Cow", creator, true,
				true, false);
		assertFalse(dag_.findOrCreateEdge(creator, false, genls, cow, mammal) instanceof ErrorEdge);
		assertNotNull(sut_.execute(true, cow, mammal));
		assertNotNull(sut_.execute(true, cow, animal));
		assertNotNull(sut_.execute(true, cow, thing));
	}

	@Test
	public void testRemoval() {
		Node creator = new StringNode("TestCreator");
		setUpDAG(creator);

		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true,
				true, false);
		DAGNode canis = (DAGNode) dag_.findOrCreateNode("Canis", creator, true,
				true, false);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true,
				true, false);
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator,
				true, true, false);
		DAGNode pet = (DAGNode) dag_.findOrCreateNode("Pet", creator, true,
				true, false);
		sut_.initialisationComplete(dag_.getNodes(), dag_.getEdges());

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
		Node thing = dag_.findOrCreateNode("Thing", creator, true, true, false);
		Node dog = dag_.findOrCreateNode("Dog", creator, true, true, false);
		Node fluffy = dag_.findOrCreateNode("Fluffy", creator, true, true,
				false);
		Node mammal = dag_.findOrCreateNode("Mammal", creator, true, true,
				false);
		Node canis = dag_.findOrCreateNode("Canis", creator, true, true, false);
		Node pet = dag_.findOrCreateNode("Pet", creator, true, true, false);
		Node cat = dag_.findOrCreateNode("Cat", creator, true, true, false);
		Node plant = dag_.findOrCreateNode("Plant", creator, true, true, false);
		dag_.findOrCreateEdge(creator, false, genls, dog, thing);
		dag_.findOrCreateEdge(creator, false, genls, dog, canis);
		dag_.findOrCreateEdge(creator, false, genls, dog, pet);
		dag_.findOrCreateEdge(creator, false, genls, cat, pet);
		dag_.findOrCreateEdge(creator, false, genls, pet, thing);
		dag_.findOrCreateEdge(creator, false, genls, canis, mammal);
		dag_.findOrCreateEdge(creator, false, genls, plant, thing);
		dag_.findOrCreateEdge(creator, false, genls, cat, fluffy);
		dag_.findOrCreateEdge(creator, false, genls, mammal, thing);
		dag_.findOrCreateEdge(creator, false, genls, cat, mammal);
		dag_.noChecks_ = false;
		sut_.initMembers();
	}

}
