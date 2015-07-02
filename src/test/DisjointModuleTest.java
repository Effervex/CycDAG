package test;

import static org.junit.Assert.*;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
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

		// Basic disjointness
		assertEquals(querier_.prove(false, disjoint, dog, cat), QueryResult.NIL);
		Edge disjDogCat = dag_.findOrCreateEdge(
				new Node[] { disjoint, dog, cat }, null, true, false);
		assertTrue(disjDogCat instanceof DAGEdge);
		QueryObject qo = new QueryObject(true, true, QueryResult.ALL, disjoint,
				dog, cat);
		assertEquals(querier_.prove(qo), QueryResult.TRUE);
		List<Node[]> justification = qo.getJustification();
		assertEquals(justification.size(), 1);

		DAGNode boxer = (DAGNode) dag_.findOrCreateNode("Boxer-Dog", null,
				true, false, true);
		dag_.findOrCreateEdge(new Node[] { isa, boxer, collection }, null,
				true, false);

		// Transitive disjointness
		Edge genlsBoxerDog = dag_.findOrCreateEdge(new Node[] { genls, boxer,
				dog }, null, true, false);
		qo = new QueryObject(true, true, QueryResult.ALL, disjoint, boxer, dog);
		assertEquals(querier_.prove(qo), QueryResult.FALSE);
		justification = qo.getJustification();
		assertEquals(justification.size(), 1);

		qo = new QueryObject(true, true, QueryResult.ALL, disjoint, boxer, cat);
		assertEquals(querier_.prove(qo), QueryResult.TRUE);
		justification = qo.getJustification();
		assertEquals(justification.size(), 2);

		// Double transitive
		DAGNode housecat = (DAGNode) dag_.findOrCreateNode("HouseCat", null,
				true, false, true);
		dag_.findOrCreateEdge(new Node[] { isa, housecat, collection }, null,
				true, false);
		dag_.findOrCreateEdge(new Node[] { genls, housecat, cat }, null, true,
				false);
		qo = new QueryObject(true, true, QueryResult.ALL, disjoint, boxer,
				housecat);
		assertEquals(querier_.prove(qo), QueryResult.TRUE);
		justification = qo.getJustification();
		assertEquals(justification.size(), 3);

		// Removing edges
		dag_.removeEdge(genlsBoxerDog);
		assertEquals(querier_.prove(false, disjoint, dog, cat),
				QueryResult.TRUE);
		assertEquals(querier_.prove(false, disjoint, boxer, cat),
				QueryResult.NIL);
		assertEquals(querier_.prove(false, disjoint, boxer, housecat),
				QueryResult.NIL);

		assertTrue(dag_.removeEdge(disjDogCat));
		assertEquals(querier_.prove(false, disjoint, dog, cat), QueryResult.NIL);
		assertEquals(querier_.prove(false, disjoint, boxer, cat),
				QueryResult.NIL);
		assertEquals(querier_.prove(false, disjoint, dog, housecat),
				QueryResult.NIL);

		// Remove genls without compromising disjoint
		genlsBoxerDog = dag_.findOrCreateEdge(new Node[] { genls, boxer, dog }, null, true,
				false);
		disjDogCat = dag_.findOrCreateEdge(
				new Node[] { disjoint, dog, cat }, null, true, false);
		assertEquals(querier_.prove(false, disjoint, dog, cat),
				QueryResult.TRUE);
		assertEquals(querier_.prove(false, disjoint, boxer, cat),
				QueryResult.TRUE);
		assertEquals(querier_.prove(false, disjoint, boxer, housecat),
				QueryResult.TRUE);
		dag_.findOrCreateEdge(
				new Node[] { disjoint, boxer, housecat}, null, true, false);
		// Remove the genls, lose the disjoint to Cat
		dag_.removeEdge(genlsBoxerDog);
		assertEquals(querier_.prove(false, disjoint, boxer, cat),
				QueryResult.NIL);
		assertEquals(querier_.prove(false, disjoint, boxer, housecat),
				QueryResult.TRUE);
	}
}
