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
	ARG1GENL("arg1Genl"),
	ARG1ISA("arg1Isa"),
	ARG1NOTGENL("arg1NotGenl"),
	ARG1NOTISA("arg1NotIsa"),
	ARG2GENL("arg2Genl"),
	ARG2ISA("arg2Isa"),
	ARG2NOTGENL("arg2NotGenl"),
	ARG2NOTISA("arg2NotIsa"),
	ARGGENL("argGenl"),
	ARGISA("argIsa"),
	ARGNOTGENL("argNotGenl"),
	ARGNOTISA("argNotIsa"),
	ARITY("arity"),
	ASSERTED_SENTENCE("assertedSentence"),
	BINARY_PREDICATE("BinaryPredicate"),
	BROADER_TERM("broaderTerm"),
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
	PRETTY_STRING("prettyString"),
	PRETTY_STRING_CANONICAL("prettyString-Canonical"),
	QUOTED_ISA("quotedIsa"),
	RESULT_GENL("resultGenl"),
	RESULT_GENL_ARG("resultGenlArg"),
	RESULT_ISA("resultIsa"),
	RESULT_ISA_ARG("resultIsaArg"),
	REWRITE_OF("rewriteOf"),
	SIBLING_DISJOINT_COLLECTION_TYPE("SiblingDisjointCollectionType"),
	SIBLING_DISJOINT_EXCEPTION("siblingDisjointExceptions"),
	STRICTLY_FUNCTIONAL_SLOT("StrictlyFunctionalSlot"),
	STRING("CharacterString"),
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

	private int setID_ = -1;

	private CommonConcepts(String nodeName) {
		nodeName_ = nodeName;
	}

	public void clearNode() {
		node_ = null;
	}

	public int getID() {
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

	public void setID(int id) {
		if (node_ == null)
			setID_ = id;
	}

	@Override
	public String toString() {
		return getID() + "";
	}

	private static void nlpPredicates(CommonConcepts cc, String nlpPred,
			CycDAG dag) {
		dag.findOrCreateEdge(
				new Node[] { NLP_PREDICATE_STRING.getNode(dag),
						cc.getNode(dag), new StringNode(nlpPred) },
				_COMMON_CONCEPT, true);
	}

	public static void createCommonAssertions(CycDAG dag) {
		boolean checks = dag.noChecks_;
		dag.noChecks_ = true;
		// isa, genls
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), ISA.getNode(dag),
				BINARY_PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), ISA.getNode(dag),
						PrimitiveNode.parseNode("2"), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), GENLS.getNode(dag),
				BINARY_PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), GENLS.getNode(dag),
						PrimitiveNode.parseNode("1"), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), GENLS.getNode(dag),
						PrimitiveNode.parseNode("2"), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { GENLS.getNode(dag), BINARY_PREDICATE.getNode(dag),
						PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);

		// Individual
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), INDIVIDUAL.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { DISJOINTWITH.getNode(dag),
						INDIVIDUAL.getNode(dag), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);

		// Collection
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), COLLECTION.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), THING.getNode(dag),
				COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { GENLS.getNode(dag), COLLECTION.getNode(dag),
						THING.getNode(dag) }, _COMMON_CONCEPT, true);

		// Predicate
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), PREDICATE.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);

		// Function
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), FUNCTION.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag),
				THE_FN.getNode(dag), FUNCTION.getNode(dag) }, _COMMON_CONCEPT,
				true);
		dag.findOrCreateEdge(
				new Node[] { RESULT_ISA_ARG.getNode(dag), THE_FN.getNode(dag),
						PrimitiveNode.parseNode("1") }, _COMMON_CONCEPT, true);

		// resultIsa/genls (+arg)
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), RESULT_GENL.getNode(dag),
						BINARY_PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_GENL.getNode(dag),
						PrimitiveNode.parseNode("1"), FUNCTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_GENL.getNode(dag),
						PrimitiveNode.parseNode("2"), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), RESULT_ISA.getNode(dag),
						BINARY_PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_ISA.getNode(dag),
						PrimitiveNode.parseNode("1"), FUNCTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_ISA.getNode(dag),
						PrimitiveNode.parseNode("2"), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), RESULT_GENL_ARG.getNode(dag),
						PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_GENL_ARG.getNode(dag),
						PrimitiveNode.parseNode("1"), FUNCTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_GENL_ARG.getNode(dag),
						PrimitiveNode.parseNode("2"),
						POSITIVE_INTEGER.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), RESULT_ISA_ARG.getNode(dag),
						PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_ISA_ARG.getNode(dag),
						PrimitiveNode.parseNode("1"), FUNCTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), RESULT_ISA_ARG.getNode(dag),
						PrimitiveNode.parseNode("2"),
						POSITIVE_INTEGER.getNode(dag) }, _COMMON_CONCEPT, true);

		// Strictly Functional
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag),
						STRICTLY_FUNCTIONAL_SLOT.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), COMMENT.getNode(dag),
						STRICTLY_FUNCTIONAL_SLOT.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), ARITY.getNode(dag),
						STRICTLY_FUNCTIONAL_SLOT.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), NLP_PREDICATE_STRING.getNode(dag),
						STRICTLY_FUNCTIONAL_SLOT.getNode(dag) },
				_COMMON_CONCEPT, true);

		// NLP Predicates
		dag.findOrCreateEdge(
				new Node[] { GENLPREDS.getNode(dag),
						PRETTY_STRING.getNode(dag), TERM_STRING.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(

				new Node[] { GENLPREDS.getNode(dag),
						PRETTY_STRING_CANONICAL.getNode(dag),
						PRETTY_STRING.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { PRETTY_STRING_CANONICAL.getNode(dag),
				PRETTY_STRING_CANONICAL.getNode(dag),
				new StringNode("commonly known as") }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(

				new Node[] { PRETTY_STRING_CANONICAL.getNode(dag),
						PRETTY_STRING.getNode(dag), new StringNode("known as") },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), CHARACTER_STRING.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { GENLS.getNode(dag), STRING.getNode(dag),
						CHARACTER_STRING.getNode(dag) }, _COMMON_CONCEPT, true);
		Node theString = dag.findOrCreateFunctionNode(true, false,
				_COMMON_CONCEPT, THE_FN.getNode(dag), STRING.getNode(dag));
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), theString, STRING.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(

		new Node[] { ISA.getNode(dag), NLP_PREDICATE_STRING.getNode(dag),
				BINARY_PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ARGISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				PrimitiveNode.parseNode("1"), PREDICATE.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ARGISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				PrimitiveNode.parseNode("2"), CHARACTER_STRING.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] {
				COMMENT.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				new StringNode(
						"A predicate for converting binary assertions into "
								+ "a natural language phrase. ("
								+ NLP_PREDICATE_STRING.nodeName_
								+ " PRED NLPSTR) means that PRED can be "
								+ "described using NLPSTR in the context of "
								+ "an adverb (e.g. 'an instance of', 'broader "
								+ "than', etc).") }, _COMMON_CONCEPT, true);

		// First Order Collection
		dag.findOrCreateEdge(new Node[] { GENLS.getNode(dag),
				FIRST_ORDER_COLLECTION.getNode(dag), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);

		// Primitives
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag),
				STRING.getNode(dag), COLLECTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), POSITIVE_INTEGER.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), NEGATIVE_INTEGER.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), ZERO.getNode(dag),
				INDIVIDUAL.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), POSITIVE_NUMBER.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), NEGATIVE_NUMBER.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), TRUE.getNode(dag),
				INDIVIDUAL.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), FALSE.getNode(dag),
				INDIVIDUAL.getNode(dag) }, _COMMON_CONCEPT, true);

		// Time functions
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), DATE.getNode(dag),
				COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag), DAYFN.getNode(dag),
				UNREIFIABLE_FUNCTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(

				new Node[] { RESULT_ISA.getNode(dag), DAYFN.getNode(dag),
						DATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), MONTHFN.getNode(dag),
						UNREIFIABLE_FUNCTION.getNode(dag) }, _COMMON_CONCEPT,
				true);
		dag.findOrCreateEdge(
				new Node[] { RESULT_ISA.getNode(dag), MONTHFN.getNode(dag),
						DATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(new Node[] { ISA.getNode(dag),
				YEARFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(

				new Node[] { RESULT_ISA.getNode(dag), YEARFN.getNode(dag),
						DATE.getNode(dag) }, _COMMON_CONCEPT, true);

		// Temporal Concepts
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), LATER_PREDICATE.getNode(dag),
						BINARY_PREDICATE.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), LATER_PREDICATE.getNode(dag),
						PrimitiveNode.parseNode("1"), DATE.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ARGISA.getNode(dag), LATER_PREDICATE.getNode(dag),
						PrimitiveNode.parseNode("2"), DATE.getNode(dag) },
				_COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), INTERVAL_BOUND.getNode(dag),
						COLLECTION.getNode(dag) }, _COMMON_CONCEPT, true);
		dag.findOrCreateEdge(
				new Node[] { ISA.getNode(dag), INTERVAL_FUNCTION.getNode(dag),
						UNREIFIABLE_FUNCTION.getNode(dag) }, _COMMON_CONCEPT,
				true);

		// $1 |1(is)|(are)| <text string> $2
		nlpPredicates(ARGGENL,
				"|2(argument $2)|(what arguments)| of $1 is a type of $3", dag);
		nlpPredicates(ARG1GENL, "argument 1 of $1 is a type of $2", dag);
		nlpPredicates(ARG2GENL, "argument 2 of $1 is a type of $2", dag);
		nlpPredicates(ARGISA,
				"|2(argument $2)|(what arguments)| of $1 is an instance of $3",
				dag);
		nlpPredicates(ARG1ISA, "argument 1 of $1 is an instance of $2", dag);
		nlpPredicates(ARG2ISA, "argument 2 of $1 is an instance of $2", dag);
		nlpPredicates(
				ARGNOTGENL,
				"|2(argument $2)|(what arguments)| of $1 cannot be a type of $3",
				dag);
		nlpPredicates(ARG1NOTGENL, "argument 1 of $1 cannot be a type of $2",
				dag);
		nlpPredicates(ARG2NOTGENL, "argument 2 of $1 cannot be a type of $2",
				dag);
		nlpPredicates(
				ARGNOTISA,
				"|2(argument $2)|(what arguments)| of $1 cannot be an instance of $3",
				dag);
		nlpPredicates(ARG1NOTISA,
				"argument 1 of $1 cannot be an instance of $2", dag);
		nlpPredicates(ARG2NOTISA,
				"argument 2 of $1 cannot be an instance of $2", dag);
		nlpPredicates(ARITY, "$1 |1(has)|(have)| arity |2($2)|(what)|", dag);
		nlpPredicates(ASSERTED_SENTENCE, "$1 |1(is)|(are)| asserted directly",
				dag);
		nlpPredicates(BROADER_TERM, "$2 |2(is)|(are)| broader than $1", dag);
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
		nlpPredicates(PRETTY_STRING, "$1 |1(is)|(are)| known as $2", dag);
		nlpPredicates(PRETTY_STRING_CANONICAL,
				"$1 |1(is)|(are)| commonly known as $2", dag);
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
		nlpPredicates(TERM_STRING, "$1 |1(is)|(are)| also known as $2", dag);
		nlpPredicates(SIBLING_DISJOINT_EXCEPTION,
				"$1 |1(is)|(are)| exempt from disjointness with $2", dag);
		nlpPredicates(REWRITE_OF,
				"$1 |1(is)|(are)| the favoured rewrite of $2", dag);
		dag.noChecks_ = checks;
	}

	public static void initialise(CycDAG dag) {
		for (CommonConcepts cc : CommonConcepts.values())
			cc.getNode(dag);
	}
}
