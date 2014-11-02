package test;

import static org.junit.Assert.*;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.inference.QueryObject;
import graph.module.DisjointModule;
import graph.module.QueryModule;

import java.io.File;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class DisjointModuleTest {
	private DisjointModule sut_;
	private CycDAG dag_;
	private QueryModule querier_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"), null, null);
		CommonConcepts.initialise(dag_);
		sut_ = (DisjointModule) dag_.getModule(DisjointModule.class);
		querier_ = (QueryModule) dag_.getModule(QueryModule.class);
		sut_.clear();
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
		sut_.clear();
	}

	@Test
	public void testModule() {
		DAGNode disjoint = CommonConcepts.DISJOINTWITH.getNode(dag_);
		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		DAGNode isa = CommonConcepts.ISA.getNode(dag_);
		DAGNode collection = CommonConcepts.COLLECTION.getNode(dag_);
		DAGNode dog = (DAGNode) dag_.findOrCreateNode("Dog", null, true, false,
				true);
		dag_.findOrCreateEdge(new Node[] { isa, dog, collection }, null, true,
				false);
		DAGNode cat = (DAGNode) dag_.findOrCreateNode("Cat", null, true, false,
				true);
		dag_.findOrCreateEdge(new Node[] { isa, cat, collection }, null, true,
				false);

		assertFalse(querier_.prove(disjoint, dog, cat));
		Edge disjDogCat = dag_.findOrCreateEdge(
				new Node[] { disjoint, dog, cat }, null, true, false);
		assertTrue(disjDogCat instanceof DAGEdge);
		QueryObject qo = new QueryObject(true, disjoint, dog, cat);
		assertTrue(querier_.prove(qo));
		List<Node[]> justification = qo.getJustification();
		assertEquals(justification.size(), 1);

		DAGNode boxer = (DAGNode) dag_.findOrCreateNode("Boxer-Dog", null,
				true, false, true);
		dag_.findOrCreateEdge(new Node[] { isa, boxer, collection }, null,
				true, false);

		dag_.findOrCreateEdge(new Node[] { genls, boxer, dog }, null, true,
				false);
		qo = new QueryObject(true, disjoint, boxer, cat);
		assertTrue(querier_.prove(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);

		DAGNode housecat = (DAGNode) dag_.findOrCreateNode("HouseCat", null,
				true, false, true);
		dag_.findOrCreateEdge(new Node[] { isa, housecat, collection }, null,
				true, false);
		dag_.findOrCreateEdge(new Node[] { genls, housecat, cat }, null, true,
				false);
		qo = new QueryObject(true, disjoint, boxer, housecat);
		assertTrue(querier_.prove(qo));
		justification = qo.getJustification();
		assertEquals(justification.size(), 3);

		assertTrue(dag_.removeEdge(disjDogCat));
		assertFalse(querier_.prove(disjoint, dog, cat));
		assertFalse(querier_.prove(disjoint, boxer, cat));
		assertFalse(querier_.prove(disjoint, boxer, housecat));
	}
}
