package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.core.PrimitiveNode;
import graph.core.StringNode;
import graph.inference.QueryObject;
import graph.inference.VariableNode;
import graph.module.NLPToStringModule;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class NLPToStringModuleTest {
	private DirectedAcyclicGraph dag_;
	private NLPToStringModule sut_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (NLPToStringModule) dag_.getModule(NLPToStringModule.class);
		assertNotNull(sut_);
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
	}

	@Test
	public void testNodeToString() {
		Node creator = new StringNode("TestCreator");
		DAGNode samuel = (DAGNode) dag_.findOrCreateNode("SamuelLJackson",
				creator, true, true, true);
		String result = sut_.nodeToString(samuel);
		assertEquals(result, "Samuel L Jackson");

		// General term string
		DAGNode alias = CommonConcepts.TERM_STRING.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, alias, samuel, new StringNode(
				"Sam Jackson"));
		result = sut_.nodeToString(samuel);
		assertEquals(result, "Sam Jackson");

		DAGNode canonical = CommonConcepts.PRETTY_STRING_CANONICAL
				.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, canonical, samuel,
				new StringNode("Samuel L. Jackson"));
		result = sut_.nodeToString(samuel);
		assertEquals(result, "Samuel L. Jackson");

		// Autonaming function
		DAGNode fruitFn = (DAGNode) dag_.findOrCreateNode("FruitFn", creator,
				true, true, true);
		DAGNode appleTree = (DAGNode) dag_.findOrCreateNode("AppleTree",
				creator, true, true, true);
		OntologyFunction apple = new OntologyFunction(fruitFn, appleTree);
		result = sut_.nodeToString(apple);
		assertEquals(result, "Apple Tree (Fruit)");

		DAGNode fermentedFn = (DAGNode) dag_.findOrCreateNode("FermentedFn",
				creator, true, true, true);
		OntologyFunction fermentedAppleTree = new OntologyFunction(fermentedFn,
				appleTree);
		result = sut_.nodeToString(fermentedAppleTree);
		assertEquals(result, "Apple Tree (Fermented)");

		// Double wrap
		OntologyFunction fermentedApple = new OntologyFunction(fermentedFn,
				apple);
		result = sut_.nodeToString(fermentedApple);
		assertEquals(result, "Apple Tree (Fruit, Fermented)");

		// Function term string
		dag_.findOrCreateEdge(creator, false, alias, fruitFn, new StringNode(
				"fruit of the ______"));
		result = sut_.nodeToString(apple);
		assertEquals(result, "fruit of the Apple Tree");

		// Double wrap term strings
		dag_.findOrCreateEdge(creator, false, alias, fermentedFn,
				new StringNode("fermented ______"));
		result = sut_.nodeToString(fermentedApple);
		assertEquals(result, "fermented fruit of the Apple Tree");

		OntologyFunction samApple = new OntologyFunction(fruitFn, samuel);
		result = sut_.nodeToString(samApple);
		assertEquals(result, "fruit of the Samuel L. Jackson");

		// Pretty string function
		dag_.findOrCreateEdge(creator, false, canonical, fermentedApple,
				new StringNode("fermented apple"));
		result = sut_.nodeToString(fermentedApple);
		assertEquals(result, "fermented apple");
	}

	@Test
	public void testEdgeToString() {
		Node creator = new StringNode("TestCreator");
		// No markup
		DAGNode isa = CommonConcepts.ISA.getNode(dag_);
		DAGNode samuel = (DAGNode) dag_.findOrCreateNode("SamuelLJackson",
				creator, true, true, true);
		DAGNode actor = (DAGNode) dag_.findOrCreateNode("Actor", creator, true,
				true, true);
		QueryObject qo = new QueryObject(isa, samuel, actor);
		String result = sut_.edgeToString(qo, false);
		assertEquals(result, "'Samuel L Jackson' has 'isa' relation to 'Actor'");
		Object[] edgeNodes = new Node[] { isa, samuel, actor };
		assertEquals(result, sut_.execute(edgeNodes));

		// Markup predicate
		DAGNode nlpPred = CommonConcepts.NLP_PREDICATE_STRING.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, nlpPred, isa, new StringNode(
				"an instance of"));
		result = sut_.edgeToString(qo, false);
		assertEquals(result, "'Samuel L Jackson' is an instance of 'Actor'");

		// Broader term
		DAGNode broader = (DAGNode) dag_.findOrCreateNode("broaderTerm",
				creator, true, true, true);
		qo = new QueryObject(broader, samuel, actor);
		result = sut_.edgeToString(qo, false);
		assertEquals(result,
				"'Samuel L Jackson' has 'broaderTerm' relation to 'Actor'");

		// Markup predicate
		dag_.findOrCreateEdge(creator, false, nlpPred, broader, new StringNode(
				"broader than"));
		result = sut_.edgeToString(qo, false);
		assertEquals(result, "'Samuel L Jackson' is broader than 'Actor'");

		// NEED TO REVERSE IT!
		DAGNode quotedIsa = CommonConcepts.QUOTED_ISA.getNode(dag_);
		DAGNode reversed = CommonConcepts.REVERSED_NLP.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, quotedIsa, broader, reversed);
		result = sut_.edgeToString(qo, false);
		assertEquals(result, "'Actor' is broader than 'Samuel L Jackson'");

		// Node strings
		DAGNode canonical = CommonConcepts.PRETTY_STRING_CANONICAL
				.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, canonical, samuel,
				new StringNode("Samuel L. Jackson"));
		dag_.findOrCreateEdge(creator, false, canonical, actor, new StringNode(
				"Actor"));
		result = sut_.edgeToString(qo, false);
		assertEquals(result, "'Actor' is broader than 'Samuel L. Jackson'");

		// Non-binary edges
		DAGNode argIsa = CommonConcepts.ARGISA.getNode(dag_);
		DAGNode thing = CommonConcepts.THING.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, argIsa, broader,
				PrimitiveNode.parseNode("1"), thing);
		qo = new QueryObject(argIsa, broader, PrimitiveNode.parseNode("1"),
				thing);
		result = sut_.edgeToString(qo, false);
		assertEquals(result,
				"'broader Term' has 'argIsa' relation to 1 and 'Thing'");
	}

	@Test
	public void testQueryToString() {
		Node creator = new StringNode("TestCreator");
		// No markup
		DAGNode isa = CommonConcepts.ISA.getNode(dag_);
		DAGNode samuel = (DAGNode) dag_.findOrCreateNode("SamuelLJackson",
				creator, true, true, true);
		DAGNode actor = (DAGNode) dag_.findOrCreateNode("Actor", creator, true,
				true, true);
		QueryObject qo = new QueryObject(isa, samuel, actor);
		String result = sut_.edgeToString(qo, true);
		assertEquals(result,
				"does 'Samuel L Jackson' have 'isa' relation to 'Actor'?");

		// Substitutions
		qo = new QueryObject(isa, samuel, VariableNode.DEFAULT);
		result = sut_.edgeToString(qo, true);
		assertEquals(result,
				"'Samuel L Jackson' has 'isa' relation to what things?");
		qo = new QueryObject(isa, VariableNode.DEFAULT, actor);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "what things have 'isa' relation to 'Actor'?");

		// Markup predicate
		DAGNode nlpPred = CommonConcepts.NLP_PREDICATE_STRING.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, nlpPred, isa, new StringNode(
				"an instance of"));
		qo = new QueryObject(isa, samuel, actor);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "is 'Samuel L Jackson' an instance of 'Actor'?");

		// Broader term
		DAGNode broader = (DAGNode) dag_.findOrCreateNode("broaderTerm",
				creator, true, true, true);
		dag_.findOrCreateEdge(creator, false, nlpPred, broader, new StringNode(
				"broader than"));
		DAGNode quotedIsa = CommonConcepts.QUOTED_ISA.getNode(dag_);
		DAGNode reversed = CommonConcepts.REVERSED_NLP.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, quotedIsa, broader, reversed);
		qo = new QueryObject(broader, samuel, actor);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "is 'Actor' broader than 'Samuel L Jackson'?");

		// Substitutions
		qo = new QueryObject(isa, samuel, VariableNode.DEFAULT);
		result = sut_.edgeToString(qo, true);
		assertEquals(result,
				"'Samuel L Jackson' is an instance of what things?");
		qo = new QueryObject(isa, new VariableNode("?Y"), actor);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "what things are an instance of 'Actor'?");
		qo = new QueryObject(broader, samuel, VariableNode.DEFAULT);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "what things are broader than 'Samuel L Jackson'?");
		qo = new QueryObject(broader, new VariableNode("?Y"), actor);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "'Actor' is broader than what things?");

		// Conjunctions
		OntologyFunction firstPart = new OntologyFunction(isa, VariableNode.DEFAULT,
				actor);
		OntologyFunction secondPart = new OntologyFunction(broader, VariableNode.DEFAULT,
				new VariableNode("?Y"));
		DAGNode and = CommonConcepts.AND.getNode(dag_);
		qo = new QueryObject(and, firstPart, secondPart);
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "what things ?X are an instance of 'Actor' AND "
				+ "what things ?Y are broader than ?X?");

		// Multi-conjunctions
		DAGNode or = CommonConcepts.OR.getNode(dag_);
		qo = new QueryObject(or, firstPart, new OntologyFunction(and, new OntologyFunction(isa, samuel, actor),
				new OntologyFunction(broader, VariableNode.DEFAULT, new VariableNode("?Y"))));
		result = sut_.edgeToString(qo, true);
		assertEquals(result, "what things ?X are an instance of 'Actor' OR "
				+ "is 'Samuel L Jackson' an instance of 'Actor' AND "
				+ "what things ?Y are broader than ?X?");
	}
}
