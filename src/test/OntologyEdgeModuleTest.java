package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.StringNode;
import graph.module.OntologyEdgeModule;

import java.io.File;
import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class OntologyEdgeModuleTest {
	private OntologyEdgeModule sut_;
	private DirectedAcyclicGraph dag_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (OntologyEdgeModule) dag_.getModule(OntologyEdgeModule.class);
		sut_.clear();
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
		sut_.clear();
	}

	@Test
	public void testFindEdgeByNodes() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true,
				true, true);
		DAGNode place = (DAGNode) dag_.findOrCreateNode("Place", creator, true,
				true, true);
		DAGNode diffFn = (DAGNode) dag_.findOrCreateNode(
				"CollectionDifferenceFn", creator, true, true, true);
		DAGNode thing = (DAGNode) dag_.findOrCreateNode("Thing", creator, true,
				true, true);
		DAGNode person = (DAGNode) dag_.findOrCreateNode("Person", creator,
				true, true, true);
		DAGEdge funcEdge = (DAGEdge) dag_.findOrCreateEdge(creator, false,
				genls, place, new OntologyFunction(diffFn, thing, person));
		Collection<Edge> results = sut_.findEdgeByNodes(genls, place,
				new OntologyFunction(diffFn, thing));
		assertEquals(results.size(), 1);
		assertTrue(results.contains(funcEdge));

		results = sut_.execute(new OntologyFunction(diffFn, thing));
		assertEquals(results.size(), 1);
		assertTrue(results.contains(funcEdge));

		// Non DAG nodes
		DAGNode cityNamedFn = (DAGNode) dag_.findOrCreateNode("CityNamedFn",
				creator, true, true, true);
		DAGNode nz = (DAGNode) dag_.findOrCreateNode("NewZealand", creator,
				true, true, true);
		DAGEdge strEdge = (DAGEdge) dag_.findOrCreateEdge(creator, false,
				genls, new OntologyFunction(cityNamedFn, new StringNode(
						"Hamilton"), nz), place);
		results = sut_.findEdgeByNodes(genls, new OntologyFunction(cityNamedFn,
				new StringNode("Auckland"), nz), place);
		assertEquals(results.size(), 0);

		results = sut_.findEdgeByNodes(genls, new OntologyFunction(cityNamedFn,
				new StringNode("Hamilton"), nz));
		assertEquals(results.size(), 1);
		assertTrue(results.contains(strEdge));
	}

	@Test
	public void testStringFunction() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true,
				true, true);
		DAGNode fn = (DAGNode) dag_.findOrCreateNode("TheFn", creator, true,
				true, true);
		DAGNode blah = (DAGNode) dag_.findOrCreateNode("Blah", creator, true,
				true, true);
		DAGNode thingy = (DAGNode) dag_.findOrCreateNode("Thing", creator,
				true, true, true);
		DAGEdge funcEdge = (DAGEdge) dag_.findOrCreateEdge(creator, false,
				genls, blah, new OntologyFunction(fn, thingy, new StringNode(
						"ABC")));
		DAGEdge funcEdge2 = (DAGEdge) dag_.findOrCreateEdge(creator, false,
				genls, blah, new OntologyFunction(fn, thingy, new StringNode(
						"123")));
		Collection<Edge> results = sut_.execute(new OntologyFunction(fn,
				thingy, new StringNode("123")), 3);
		assertEquals(results.toString(), results.size(), 1);
		assertTrue(results.contains(funcEdge2));
	}

	@Test
	public void testRemovedEdges() {
		Node creator = new StringNode("TestCreator");
		DAGNode genls = (DAGNode) dag_.findOrCreateNode("genls", creator, true,
				true, true);
		DAGNode isa = (DAGNode) dag_.findOrCreateNode("isa", creator, true,
				true, true);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", creator, true,
				true, true);
		DAGNode domesticated = (DAGNode) dag_.findOrCreateNode("Domesticated",
				creator, true, true, true);
		DAGNode mammal = (DAGNode) dag_.findOrCreateNode("Mammal", creator,
				true, true, true);
		DAGEdge isaEdge = (DAGEdge) dag_.findOrCreateEdge(creator, false, isa,
				dog, domesticated);
		DAGEdge genlsEdge = (DAGEdge) dag_.findOrCreateEdge(creator, false,
				genls, dog, mammal);
		Collection<Edge> results = sut_.execute(dog);
		assertEquals(results.toString(), results.size(), 2);
		assertTrue(results.contains(isaEdge));
		assertTrue(results.contains(genlsEdge));
		
		results = sut_.execute(dog, isa, -1);
		assertEquals(results.toString(), results.size(), 1);
		assertTrue(results.contains(genlsEdge));
		
		results = sut_.execute(dog, genls, -1);
		assertEquals(results.toString(), results.size(), 1);
		assertTrue(results.contains(isaEdge));
	}
}
