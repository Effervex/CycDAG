package test;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Collection;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.PrimitiveNode;
import graph.core.StringNode;
import graph.module.PredicateRefinerModule;
import graph.module.RelatedEdgeModule;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

public class PredicateRefinerModuleTest {
	private static CycDAG dag_;
	private static PredicateRefinerModule sut_;
	private static RelatedEdgeModule relEdgeModule_;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		dag_ = new CycDAG(new File("test"), null, null);
		CommonConcepts.initialise(dag_);
		sut_ = (PredicateRefinerModule) dag_
				.getModule(PredicateRefinerModule.class);
		relEdgeModule_ = (RelatedEdgeModule) dag_
				.getModule(RelatedEdgeModule.class);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testInferConstraints() {
		Node creator = new StringNode("TestCreator");
		Node visualAppearance = dag_.findOrCreateNode("appearance", creator,
				true);
		Node owns = dag_.findOrCreateNode("owns", creator, true);
		DAGNode isa = CommonConcepts.ISA.getNode(dag_);
		DAGNode genls = CommonConcepts.GENLS.getNode(dag_);
		dag_.findOrCreateEdge(new Node[] { isa, visualAppearance,
				CommonConcepts.REFINABLE_PREDICATE.getNode(dag_) }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { isa, owns,
				CommonConcepts.REFINABLE_PREDICATE.getNode(dag_) }, creator,
				true);

		// Set up DAG
		Node collection = dag_.findOrCreateNode("Collection", creator, true);
		Node person = dag_.findOrCreateNode("Person", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, person, collection }, creator,
				true);
		Node mammal = dag_.findOrCreateNode("Mammal", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, mammal, collection }, creator,
				true);
		Node dog = dag_.findOrCreateNode("Dog", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, dog, collection }, creator,
				true);
		Node animal = dag_.findOrCreateNode("Animal", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, animal, collection }, creator,
				true);
		Node appearance = dag_.findOrCreateNode("Appearance", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, appearance, collection },
				creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, person, mammal }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, dog, mammal }, creator, true);
		dag_.findOrCreateEdge(new Node[] { genls, mammal, animal }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { genls, appearance, animal },
				creator, true);

		// Add test data
		Node fido = dag_.findOrCreateNode("Fido", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, fido, dog }, creator, true);
		Node cute = dag_.findOrCreateNode("Cute", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, cute, appearance }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { visualAppearance, fido, cute },
				creator, true);
		Node spot = dag_.findOrCreateNode("Spot", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, spot, dog }, creator, true);
		Node ugly = dag_.findOrCreateNode("Ugly", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, ugly, appearance }, creator,
				true);
		dag_.findOrCreateEdge(new Node[] { visualAppearance, spot, ugly },
				creator, true);
		Node bowser = dag_.findOrCreateNode("Bowser", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, bowser, dog }, creator, true);
		dag_.findOrCreateEdge(new Node[] { visualAppearance, bowser, cute },
				creator, true);
		Node jim = dag_.findOrCreateNode("Jim", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, jim, person }, creator, true);
		dag_.findOrCreateEdge(new Node[] { owns, jim, fido }, creator, true);

		// Infer the constraints
		Collection<Edge> refEdges = relEdgeModule_.execute(visualAppearance, 1);
		sut_.execute(3, 0.95);
		Edge e = dag_
				.findOrCreateEdge(
						new Node[] { CommonConcepts.ARGISA.getNode(dag_),
								visualAppearance,
								PrimitiveNode.parseNode(1 + ""), dog }, null,
						false);
		assertNotNull(e);
		dag_.removeEdge(e, true);
		e = dag_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(dag_),
						visualAppearance, PrimitiveNode.parseNode(2 + ""),
						appearance }, null, false);
		// Not enough evidence
		assertNull(e);
		assertNotNull(dag_
				.findOrCreateEdge(new Node[] { isa, visualAppearance,
						CommonConcepts.REFINABLE_PREDICATE.getNode(dag_) },
						null, false));
		assertTrue(relEdgeModule_.execute(CommonConcepts.ARG1ISA.getNode(dag_), 1, owns, 2)
				.isEmpty());

		// General constraint
		Node uggo = dag_.findOrCreateNode("Uggo", creator, true);
		dag_.findOrCreateEdge(new Node[] { isa, uggo, person }, creator, true);
		dag_.findOrCreateEdge(new Node[] { visualAppearance, uggo, ugly },
				creator, true);
		refEdges = relEdgeModule_.execute(visualAppearance, 1);
		sut_.execute(2, 0.95);
		// Won't exist due to global constraint
		e = dag_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(dag_),
						visualAppearance, PrimitiveNode.parseNode(1 + ""),
						mammal }, null, false);
		assertNull(e);
		e = dag_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(dag_),
						visualAppearance, PrimitiveNode.parseNode(2 + ""),
						appearance }, null, false);
		assertNotNull(e);
		dag_.removeEdge(e, true);
		assertNotNull(dag_
				.findOrCreateEdge(new Node[] { isa, visualAppearance,
						CommonConcepts.REFINABLE_PREDICATE.getNode(dag_) },
						null, false));

		// Loose threshold
		refEdges = relEdgeModule_.execute(visualAppearance, 1);
		sut_.execute(3, 0.75);
		e = dag_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(dag_),
						visualAppearance, PrimitiveNode.parseNode(1 + ""), dog },
				null, false);
		assertNotNull(e);
		dag_.removeEdge(e, true);
		e = dag_.findOrCreateEdge(
				new Node[] { CommonConcepts.ARGISA.getNode(dag_),
						visualAppearance, PrimitiveNode.parseNode(2 + ""),
						appearance }, null, false);
		// Won't find appearance due to relaxed global identification
		assertNull(e);
		assertNotNull(dag_
				.findOrCreateEdge(new Node[] { isa, visualAppearance,
						CommonConcepts.REFINABLE_PREDICATE.getNode(dag_) },
						null, false));

		// TODO Too loose constraints
	}
}
