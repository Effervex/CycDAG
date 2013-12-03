package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.ErrorEdge;
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
	private CycDAG dag_;
	private NLPToStringModule sut_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (NLPToStringModule) dag_.getModule(NLPToStringModule.class);
		assertNotNull(sut_);
		CommonConcepts.initialise(dag_);
		CommonConcepts.createCommonAssertions(dag_);
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
		String result = sut_.nodeToString(samuel, false);
		assertEquals(result, "Samuel L Jackson");

		// General term string
		DAGNode alias = CommonConcepts.TERM_STRING.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, alias, samuel, new StringNode(
				"Sam Jackson"));
		result = sut_.nodeToString(samuel, false);
		assertEquals(result, "Sam Jackson");

		DAGNode canonical = CommonConcepts.PRETTY_STRING_CANONICAL
				.getNode(dag_);
		dag_.findOrCreateEdge(creator, false, canonical, samuel,
				new StringNode("Samuel L. Jackson"));
		result = sut_.nodeToString(samuel, false);
		assertEquals(result, "Samuel L. Jackson");

		// Autonaming function
		DAGNode fruitFn = (DAGNode) dag_.findOrCreateNode("FruitFn", creator,
				true, true, true);
		DAGNode appleTree = (DAGNode) dag_.findOrCreateNode("AppleTree",
				creator, true, true, true);
		OntologyFunction apple = new OntologyFunction(dag_, fruitFn, appleTree);
		result = sut_.nodeToString(apple, false);
		assertEquals(result, "Apple Tree (Fruit)");

		DAGNode fermentedFn = (DAGNode) dag_.findOrCreateNode("FermentedFn",
				creator, true, true, true);
		OntologyFunction fermentedAppleTree = new OntologyFunction(dag_,
				fermentedFn, appleTree);
		result = sut_.nodeToString(fermentedAppleTree, false);
		assertEquals(result, "Apple Tree (Fermented)");

		// Double wrap
		OntologyFunction fermentedApple = new OntologyFunction(dag_,
				fermentedFn, apple);
		result = sut_.nodeToString(fermentedApple, false);
		assertEquals(result, "Apple Tree (Fruit, Fermented)");

		// Function term string
		dag_.findOrCreateEdge(creator, false, alias, fruitFn, new StringNode(
				"fruit of the ______"));
		result = sut_.nodeToString(apple, false);
		assertEquals(result, "fruit of the Apple Tree");

		// Double wrap term strings
		dag_.findOrCreateEdge(creator, false, alias, fermentedFn,
				new StringNode("fermented ______"));
		result = sut_.nodeToString(fermentedApple, false);
		assertEquals(result, "fermented fruit of the Apple Tree");

		OntologyFunction samApple = new OntologyFunction(dag_, fruitFn, samuel);
		result = sut_.nodeToString(samApple, false);
		assertEquals(result, "fruit of the Samuel L. Jackson");

		// Pretty string function
		dag_.findOrCreateEdge(creator, false, canonical, fermentedApple,
				new StringNode("fermented apple"));
		result = sut_.nodeToString(fermentedApple, false);
		assertEquals(result, "fermented apple");
	}

	@Test
	public void testEdgeToString() {
		Node creator = new StringNode("TestCreator");
		// No markup
		DAGNode isa2 = (DAGNode) dag_.findOrCreateNode("isa2", creator, true,
				true, true);
		assertFalse(dag_.findOrCreateEdge(creator, false,
				CommonConcepts.ISA.getNode(dag_), isa2,
				CommonConcepts.PREDICATE.getNode(dag_)) instanceof ErrorEdge);
		DAGNode samuel = (DAGNode) dag_.findOrCreateNode("SamuelLJackson",
				creator, true, true, true);
		DAGNode actor = (DAGNode) dag_.findOrCreateNode("Actor", creator, true,
				true, true);
		QueryObject qo = new QueryObject(isa2, samuel, actor);
		String result = sut_.edgeToString(qo, false, false, false);
		assertEquals(result,
				"'Samuel L Jackson' has 'isa2' relation to 'Actor'");
		Object[] edgeNodes = new Node[] { isa2, samuel, actor };
		assertEquals(result, sut_.execute(edgeNodes));

		// Markup predicate
		DAGNode nlpPred = CommonConcepts.NLP_PREDICATE_STRING.getNode(dag_);
		assertFalse(dag_.findOrCreateEdge(creator, false, nlpPred, isa2,
				new StringNode("$1 |1(is)|(are)| an instance of $2")) instanceof ErrorEdge);
		result = sut_.edgeToString(qo, false, false, false);
		assertEquals(result, "'Samuel L Jackson' is an instance of 'Actor'");

		// Broader term
		DAGNode broader = (DAGNode) dag_.findOrCreateNode("broaderTerm",
				creator, true, true, true);
		assertFalse(dag_.findOrCreateEdge(creator, false,
				CommonConcepts.ISA.getNode(dag_), broader,
				CommonConcepts.PREDICATE.getNode(dag_)) instanceof ErrorEdge);
		qo = new QueryObject(broader, samuel, actor);
		result = sut_.edgeToString(qo, false, false, false);
		assertEquals(result,
				"'Samuel L Jackson' has 'broaderTerm' relation to 'Actor'");

		// Markup predicate
		assertFalse(dag_.findOrCreateEdge(creator, false, nlpPred, broader,
				new StringNode("$2 |2(is)|(are)| broader than $1")) instanceof ErrorEdge);
		result = sut_.edgeToString(qo, false, false, false);
		assertEquals(result, "'Actor' is broader than 'Samuel L Jackson'");

		// Node strings
		DAGNode canonical = CommonConcepts.PRETTY_STRING_CANONICAL
				.getNode(dag_);
		assertFalse(dag_.findOrCreateEdge(creator, false, canonical, samuel,
				new StringNode("Samuel L. Jackson")) instanceof ErrorEdge);
		assertFalse(dag_.findOrCreateEdge(creator, false, canonical, actor,
				new StringNode("Actor")) instanceof ErrorEdge);
		result = sut_.edgeToString(qo, false, false, false);
		assertEquals(result, "'Actor' is broader than 'Samuel L. Jackson'");

		// Non-binary edges
		DAGNode argIsa2 = (DAGNode) dag_.findOrCreateNode("argIsa2", creator,
				true, true, true);
		assertFalse(dag_.findOrCreateEdge(creator, false,
				CommonConcepts.ISA.getNode(dag_), argIsa2,
				CommonConcepts.PREDICATE.getNode(dag_)) instanceof ErrorEdge);
		DAGNode thing = CommonConcepts.THING.getNode(dag_);
		assertFalse(dag_.findOrCreateEdge(creator, false, argIsa2, broader,
				PrimitiveNode.parseNode("1"), thing) instanceof ErrorEdge);
		qo = new QueryObject(argIsa2, broader, PrimitiveNode.parseNode("1"),
				thing);
		result = sut_.edgeToString(qo, false, false, false);
		assertEquals(result,
				"'broader Term' has 'argIsa2' relation to 1 and 'Thing'");
	}

	@Test
	public void testQueryToString() {
		Node creator = new StringNode("TestCreator");
		// No markup
		DAGNode isa2 = (DAGNode) dag_.findOrCreateNode("isa2", creator, true,
				true, true);
		assertFalse(dag_.findOrCreateEdge(creator, false,
				CommonConcepts.ISA.getNode(dag_), isa2,
				CommonConcepts.PREDICATE.getNode(dag_)) instanceof ErrorEdge);
		DAGNode samuel = (DAGNode) dag_.findOrCreateNode("SamuelLJackson",
				creator, true, true, true);
		DAGNode actor = (DAGNode) dag_.findOrCreateNode("Actor", creator, true,
				true, true);
		QueryObject qo = new QueryObject(isa2, samuel, actor);
		String result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result,
				"does 'Samuel L Jackson' have 'isa2' relation to 'Actor'?");

		// Substitutions
		qo = new QueryObject(isa2, samuel, VariableNode.DEFAULT);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result,
				"'Samuel L Jackson' has 'isa2' relation to what things?");
		qo = new QueryObject(isa2, VariableNode.DEFAULT, actor);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "what things have 'isa2' relation to 'Actor'?");

		// Markup predicate
		DAGNode nlpPred = CommonConcepts.NLP_PREDICATE_STRING.getNode(dag_);
		assertFalse(dag_.findOrCreateEdge(creator, false, nlpPred, isa2,
				new StringNode("$1 |1(is)|(are)| an instance of $2")) instanceof ErrorEdge);
		qo = new QueryObject(isa2, samuel, actor);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "is 'Samuel L Jackson' an instance of 'Actor'?");

		// Broader term
		DAGNode broader = (DAGNode) dag_.findOrCreateNode("broaderTerm",
				creator, true, true, true);
		assertFalse(dag_.findOrCreateEdge(creator, false,
				CommonConcepts.ISA.getNode(dag_), broader,
				CommonConcepts.PREDICATE.getNode(dag_)) instanceof ErrorEdge);
		assertFalse(dag_.findOrCreateEdge(creator, false, nlpPred, broader,
				new StringNode("$2 |2(is)|(are)| broader than $1")) instanceof ErrorEdge);
		qo = new QueryObject(broader, samuel, actor);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "is 'Actor' broader than 'Samuel L Jackson'?");

		// Substitutions
		qo = new QueryObject(isa2, samuel, VariableNode.DEFAULT);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result,
				"'Samuel L Jackson' is an instance of what things?");
		qo = new QueryObject(isa2, new VariableNode("?Y"), actor);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "what things are an instance of 'Actor'?");
		qo = new QueryObject(broader, samuel, VariableNode.DEFAULT);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "what things are broader than 'Samuel L Jackson'?");
		qo = new QueryObject(broader, new VariableNode("?Y"), actor);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "'Actor' is broader than what things?");

		// Conjunctions
		OntologyFunction firstPart = new OntologyFunction(true, isa2,
				VariableNode.DEFAULT, actor);
		OntologyFunction secondPart = new OntologyFunction(true, broader,
				VariableNode.DEFAULT, new VariableNode("?Y"));
		DAGNode and = CommonConcepts.AND.getNode(dag_);
		qo = new QueryObject(and, firstPart, secondPart);
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "what things ?X are an instance of 'Actor' AND "
				+ "what things ?Y are broader than what things ?X?");

		// Multi-conjunctions
		DAGNode or = CommonConcepts.OR.getNode(dag_);
		qo = new QueryObject(or, firstPart, new OntologyFunction(true, and,
				new OntologyFunction(true, isa2, samuel, actor),
				new OntologyFunction(true, broader, VariableNode.DEFAULT,
						new VariableNode("?Y"))));
		result = sut_.edgeToString(qo, true, false, false);
		assertEquals(result, "what things ?X are an instance of 'Actor' OR "
				+ "is 'Samuel L Jackson' an instance of 'Actor' AND "
				+ "what things ?Y are broader than what things ?X?");
	}

	@Test
	public void testPrintAllEdge() {
		StringNode strA = new StringNode("A");
		StringNode strB = new StringNode("B");
		StringNode strC = new StringNode("C");
		VariableNode var = new VariableNode("?X");
		for (CommonConcepts cc : CommonConcepts.values()) {
			if (!Character.isLowerCase(cc.getNodeName().charAt(0)))
				continue;
			try {
				// Edge
				QueryObject queryObject = new QueryObject(cc.getNode(dag_),
						strA, strB, strC);
				System.out.println("Edge: " + queryObject.toString() + "\n\t"
						+ sut_.edgeToString(queryObject, false, false, false));
				// Proof
				System.out.println("Query (proof): " + queryObject.toString()
						+ "\n\t"
						+ sut_.edgeToString(queryObject, true, false, false));
				// Var A
				queryObject = new QueryObject(cc.getNode(dag_), var, strB, strC);
				System.out.println("Query (1): " + queryObject.toString()
						+ "\n\t"
						+ sut_.edgeToString(queryObject, true, false, false));
				// Var B
				queryObject = new QueryObject(cc.getNode(dag_), strA, var, strC);
				System.out.println("Query (2): " + queryObject.toString()
						+ "\n\t"
						+ sut_.edgeToString(queryObject, true, false, false));
				// Var C
				queryObject = new QueryObject(cc.getNode(dag_), strA, strB, var);
				System.out.println("Query (3): " + queryObject.toString()
						+ "\n\t"
						+ sut_.edgeToString(queryObject, true, false, false));
			} catch (Exception e) {
				System.out
						.println("Could not print '" + cc.getNodeName() + "'");
			}
			System.out.println();
		}

		// And and Or
		QueryObject queryObject = new QueryObject(
				CommonConcepts.AND.getNode(dag_), new OntologyFunction(true,
						CommonConcepts.ISA.getNode(dag_), strA, strB),
				new OntologyFunction(true, CommonConcepts.GENLS.getNode(dag_),
						strB, strC));
		System.out.println("Edge: " + queryObject.toString() + "\n\t"
				+ sut_.edgeToString(queryObject, false, false, false));
		// Proof
		System.out.println("Query (proof): " + queryObject.toString() + "\n\t"
				+ sut_.edgeToString(queryObject, true, false, false));
		// Var A
		queryObject = new QueryObject(CommonConcepts.OR.getNode(dag_),
				new OntologyFunction(true, CommonConcepts.ISA.getNode(dag_),
						var, strB), new OntologyFunction(true,
						CommonConcepts.GENLS.getNode(dag_), strB, strC));
		System.out.println("Query (1): " + queryObject.toString() + "\n\t"
				+ sut_.edgeToString(queryObject, true, false, false));
		// Var B
		queryObject = new QueryObject(CommonConcepts.AND.getNode(dag_),
				new OntologyFunction(true, CommonConcepts.ISA.getNode(dag_),
						strA, var), new OntologyFunction(true,
						CommonConcepts.GENLS.getNode(dag_), var, strC));
		System.out.println("Query (2): " + queryObject.toString() + "\n\t"
				+ sut_.edgeToString(queryObject, true, false, false));
		// Var C
		queryObject = new QueryObject(CommonConcepts.OR.getNode(dag_),
				new OntologyFunction(true, CommonConcepts.ISA.getNode(dag_),
						strA, strB), new OntologyFunction(true,
						CommonConcepts.GENLS.getNode(dag_), strB, var));
		System.out.println("Query (3): " + queryObject.toString() + "\n\t"
				+ sut_.edgeToString(queryObject, true, false, false));
		// Two vars
		queryObject = new QueryObject(CommonConcepts.OR.getNode(dag_),
				new OntologyFunction(true, CommonConcepts.ISA.getNode(dag_),
						strA, var), new OntologyFunction(true,
						CommonConcepts.GENLS.getNode(dag_), var,
						new VariableNode("?Y")));
		System.out.println("Query (4): " + queryObject.toString() + "\n\t"
				+ sut_.edgeToString(queryObject, true, false, false));
	}
}
