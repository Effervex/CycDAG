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
package graph.core;

/**
 * A class containing commonly referenced concepts. Also contains some utility
 * methods for those common concepts.
 * 
 * @author Sam Sarjant
 */
public enum CommonConcepts {
	AND("and"),
	ARGGENL("argGenl"),
	ARGISA("argIsa"),
	ARITY("arity"),
	ASSERTED_SENTENCE("assertedSentence"),
	BINARY_PREDICATE("BinaryPredicate"),
	CHARACTER_STRING("CharacterString"),
	COLLECTION("Collection"),
	COMMENT("comment"),
	DATE("Date"),
	DAYFN("DayFn"),
	DIFFERENT("different"),
	DISJOINTWITH("disjointWith"),
	EQUALS("equals"),
	FALSE("False"),
	FIRST_ORDER_COLLECTION("FirstOrderCollection"),
	FUNCTION("Function-Denotational"),
	GENLINVERSE("genlInverse"),
	GENLMT("genlMt"),
	GENLPREDS("genlPreds"),
	GENLS("genls"),
	INDIVIDUAL("Individual"),
	INTERVAL_BOUND("TimeInterval"),
	INTERVAL_FUNCTION("TimeIntervalInclusiveFn"),
	ISA("isa"),
	LATER_PREDICATE("laterThan"),
	MONTHFN("MonthFn"),
	NEGATIVE_INTEGER("NegativeInteger"),
	NEGATIVE_NUMBER("NegativeNumber"),
	NLP_PREDICATE_STRING("nlpPredicateString"),
	NOT("not"),
	OR("or"),
	POSITIVE_INTEGER("PositiveInteger"),
	POSITIVE_NUMBER("PositiveNumber"),
	PREDICATE("Predicate"),
	PRETTY_STRING_CANONICAL("prettyString-Canonical"),
	QUOTED_ISA("quotedIsa"),
	RESULT_GENL("resultGenl"),
	RESULT_GENL_ARG("resultGenlArg"),
	RESULT_ISA("resultIsa"),
	RESULT_ISA_ARG("resultIsaArg"),
	SIBLING_DISJOINT_COLLECTION_TYPE("SiblingDisjointCollectionType"),
	SIBLING_DISJOINT_EXCEPTION("siblingDisjointExceptions"),
	STRING("ControlCharacterFreeString"),
	SYMMETRIC_BINARY("SymmetricBinaryPredicate"),
	TERM_STRING("termStrings"),
	THE_FN("TheFn"),
	THE_YEAR("TheYear-Indexical"),
	THING("Thing"),
	TRUE("True"),
	UNREIFIABLE_FUNCTION("UnreifiableFunction"),
	YEARFN("YearFn"),
	ZERO("Zero");

	private static final StringNode _COMMON_CONCEPT = new StringNode(
			"CommonConcept");

	private DAGNode node_;

	private String nodeName_;

	private long setID_ = -1;

	private CommonConcepts(String nodeName) {
		nodeName_ = nodeName;
	}

	public void clearNode() {
		node_ = null;
	}

	public long getID() {
		if (node_ == null)
			return setID_;
		return node_.id_;
	}

	public DAGNode getNode(DirectedAcyclicGraph dag) {
		if (node_ == null)
			node_ = (DAGNode) dag.findOrCreateNode(nodeName_, _COMMON_CONCEPT,
					true, false, true);
		return node_;
	}

	public String getNodeName() {
		return nodeName_;
	}

	public void setID(long id) {
		if (node_ == null)
			setID_ = id;
	}

	@Override
	public String toString() {
		return getID() + "";
	}

	private static void nlpPredicates(CommonConcepts cc, String nlpPred,
			CycDAG dag) {
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { 
				NLP_PREDICATE_STRING.getNode(dag), cc.getNode(dag),
				new StringNode(nlpPred)}, false);
	}

	public static void createCommonAssertions(CycDAG dag) {
		boolean checks = dag.noChecks_;
		dag.noChecks_ = true;
		// isa, genls
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag), ISA.getNode(dag),
				BINARY_PREDICATE.getNode(dag) }, 
				false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				ISA.getNode(dag), PrimitiveNode.parseNode("2"),
				COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				GENLS.getNode(dag), BINARY_PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				GENLS.getNode(dag), PrimitiveNode.parseNode("1"),
				COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				GENLS.getNode(dag), PrimitiveNode.parseNode("2"),
				COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { GENLS.getNode(dag),
				BINARY_PREDICATE.getNode(dag), PREDICATE.getNode(dag)}, false);

		// Individual
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				INDIVIDUAL.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { DISJOINTWITH.getNode(dag),
				INDIVIDUAL.getNode(dag), COLLECTION.getNode(dag)}, false);

		// Collection
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				COLLECTION.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				THING.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { GENLS.getNode(dag),
				COLLECTION.getNode(dag), THING.getNode(dag)}, false);

		// Predicate
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				PREDICATE.getNode(dag), COLLECTION.getNode(dag)}, false);

		// Function
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				FUNCTION.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				THE_FN.getNode(dag), FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { 
				RESULT_ISA_ARG.getNode(dag), THE_FN.getNode(dag),
				PrimitiveNode.parseNode("1")}, false);

		// resultIsa/genls (+arg)
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				RESULT_GENL.getNode(dag), BINARY_PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_GENL.getNode(dag), PrimitiveNode.parseNode("1"),
				FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_GENL.getNode(dag), PrimitiveNode.parseNode("2"),
				COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				RESULT_ISA.getNode(dag), BINARY_PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_ISA.getNode(dag), PrimitiveNode.parseNode("1"),
				FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_ISA.getNode(dag), PrimitiveNode.parseNode("2"),
				COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				RESULT_GENL_ARG.getNode(dag), PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_GENL_ARG.getNode(dag), PrimitiveNode.parseNode("1"),
				FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_GENL_ARG.getNode(dag), PrimitiveNode.parseNode("2"),
				POSITIVE_INTEGER.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				RESULT_ISA_ARG.getNode(dag), PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_ISA_ARG.getNode(dag), PrimitiveNode.parseNode("1"),
				FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				RESULT_ISA_ARG.getNode(dag), PrimitiveNode.parseNode("2"),
				POSITIVE_INTEGER.getNode(dag)}, false);

		// NLP Predicates
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				CHARACTER_STRING.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { GENLS.getNode(dag),
				STRING.getNode(dag), CHARACTER_STRING.getNode(dag)}, false);
		dag.findOrCreateEdge(
				_COMMON_CONCEPT,
				new Node[] { 
				ISA.getNode(dag),
				new OntologyFunction(dag, THE_FN.getNode(dag), STRING
						.getNode(dag)), STRING.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				BINARY_PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				PrimitiveNode.parseNode("1"), PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				PrimitiveNode.parseNode("2"), CHARACTER_STRING.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { COMMENT.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag), new StringNode(
						"A predicate for converting binary assertions into "
								+ "a natural language phrase. ("
								+ NLP_PREDICATE_STRING.nodeName_
								+ " PRED NLPSTR) means that PRED can be "
								+ "described using NLPSTR in the context of "
								+ "an adverb (e.g. 'an instance of', 'broader "
								+ "than', etc).")}, false);

		// First Order Collection
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { GENLS.getNode(dag),
				FIRST_ORDER_COLLECTION.getNode(dag), COLLECTION.getNode(dag)}, false);

		// Primitives
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				STRING.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				POSITIVE_INTEGER.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				NEGATIVE_INTEGER.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				ZERO.getNode(dag), INDIVIDUAL.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				POSITIVE_NUMBER.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				NEGATIVE_NUMBER.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				TRUE.getNode(dag), INDIVIDUAL.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				FALSE.getNode(dag), INDIVIDUAL.getNode(dag)}, false);

		// Time functions
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				DATE.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				DAYFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { RESULT_ISA.getNode(dag),
				DAYFN.getNode(dag), DATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				MONTHFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { RESULT_ISA.getNode(dag),
				MONTHFN.getNode(dag), DATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				YEARFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { RESULT_ISA.getNode(dag),
				YEARFN.getNode(dag), DATE.getNode(dag)}, false);

		// Temporal Concepts
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				LATER_PREDICATE.getNode(dag), BINARY_PREDICATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				LATER_PREDICATE.getNode(dag), PrimitiveNode.parseNode("1"),
				DATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ARGISA.getNode(dag),
				LATER_PREDICATE.getNode(dag), PrimitiveNode.parseNode("2"),
				DATE.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				INTERVAL_BOUND.getNode(dag), COLLECTION.getNode(dag)}, false);
		dag.findOrCreateEdge(_COMMON_CONCEPT, new Node[] { ISA.getNode(dag),
				INTERVAL_FUNCTION.getNode(dag),
				UNREIFIABLE_FUNCTION.getNode(dag)}, false);

		// $1 |1(is)|(are)| <text string> $2
		nlpPredicates(
				ARGGENL,
				"|2(argument $2)|(what arguments)| of $1 |1(is)|(is)| a type of $3",
				dag);
		nlpPredicates(
				ARGISA,
				"|2(argument $2)|(what arguments)| of $1 |1(is)|(is)| an instance of $3",
				dag);
		nlpPredicates(ARITY, "$1 |1(has)|(have)| arity |2($2)|(what)|", dag);
		nlpPredicates(ASSERTED_SENTENCE, "$1 |1(is)|(are)| asserted directly",
				dag);
		nlpPredicates(COMMENT,
				"$1 |1(has)|(have)| |2(comment $2)|(what comment)|", dag);
		nlpPredicates(DIFFERENT, "$1 |1(is)|(are)| different to $2", dag);
		nlpPredicates(DISJOINTWITH, "$1 |1(is)|(are)| disjoint with $2", dag);
		nlpPredicates(GENLINVERSE, "$1 |1(is)|(are)| a supertype of $2", dag);
		nlpPredicates(GENLMT, "$1 |1(is)|(are)| a sub-microtheory of $2", dag);
		nlpPredicates(GENLPREDS, "$1 |1(is)|(are)| a sub-predicate of $2", dag);
		nlpPredicates(GENLS, "$1 |1(is)|(are)| a kind of $2", dag);
		nlpPredicates(ISA, "$1 |1(is)|(are)| an instance of $2", dag);
		nlpPredicates(LATER_PREDICATE, "$1 |1(is)|(are)| later than $2", dag);
		nlpPredicates(NLP_PREDICATE_STRING,
				"$2 |2(is)|(are)| the NL predicate for $1", dag);
		nlpPredicates(NOT, "$1 |1(is)|(are)| not true", dag);
		nlpPredicates(PRETTY_STRING_CANONICAL,
				"$1 |1(is)|(are)| typically referred to as $2", dag);
		nlpPredicates(QUOTED_ISA,
				"$1 |1(is)|(are)| implicitly an instance of $2", dag);
		nlpPredicates(RESULT_GENL, "$2 |2(is)|(are)| the type produced by $1",
				dag);
		nlpPredicates(RESULT_ISA,
				"$2 |2(is)|(are)| the instance produced by $1", dag);
		nlpPredicates(
				RESULT_GENL_ARG,
				"the type produced by $1 |1(is)|(are)| the same as |2(argument $2)|(what arguments)|",
				dag);
		nlpPredicates(
				RESULT_ISA_ARG,
				"the instance produced by $1 |1(is)|(are)| the same as |2(argument $2)|(what arguments)|",
				dag);
		nlpPredicates(SIBLING_DISJOINT_EXCEPTION,
				"$1 |1(is)|(are)| exempt from disjointness with $2", dag);
		nlpPredicates(TERM_STRING, "$1 |1(is)|(are)| also known as $2", dag);
		dag.noChecks_ = checks;
	}

	public static void initialise(CycDAG dag) {
		for (CommonConcepts cc : CommonConcepts.values())
			cc.getNode(dag);
	}
}
