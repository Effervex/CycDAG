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
	ISA("isa"),
	MONTHFN("MonthFn"),
	NEGATIVE_INTEGER("(TheFn NegativeInteger)"),
	NEGATIVE_NUMBER("(TheFn NegativeNumber)"),
	NLP_PREDICATE_STRING("nlpPredicateString"),
	NOT("not"),
	OR("or"),
	POSITIVE_INTEGER("(TheFn PositiveInteger)"),
	POSITIVE_NUMBER("(TheFn PositiveNumber)"),
	PREDICATE("Predicate"),
	PRETTY_STRING_CANONICAL("prettyString-Canonical"),
	QUOTED_ISA("quotedIsa"),
	RESULTGENL("resultGenl"),
	RESULTISA("resultIsa"),
	REVERSED_NLP("ReversedNLPStringPredicate"),
	SIBLING_DISJOINT_COLLECTION_TYPE("SiblingDisjointCollectionType"),
	SIBLING_DISJOINT_EXCEPTION("siblingDisjointExceptions"),
	STRING("(TheFn ControlCharacterFreeString)"),
	SYMMETRIC_BINARY("SymmetricBinaryPredicate"),
	TERM_STRING("termStrings"),
	THEYEAR("TheYear-Indexical"),
	THING("Thing"),
	TRUE("True"),
	UNREIFIABLE_FUNCTION("UnreifiableFunction"),
	YEARFN("YearFn"),
	ZERO("Zero"),
	LATER_PREDICATE("laterThan"),
	INTERVAL_BOUND("TimeInterval"),
	INTERVAL_FUNCTION("TimeIntervalInclusiveFn");

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
					true, true, false);
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
			boolean reversed, CycDAG dag) {
		dag.findOrCreateEdge(_COMMON_CONCEPT, false,
				NLP_PREDICATE_STRING.getNode(dag), cc.getNode(dag),
				new StringNode(nlpPred));
		if (reversed)
			dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
					cc.getNode(dag), REVERSED_NLP.getNode(dag));
	}

	public static void createCommonAssertions(CycDAG dag) {
		boolean checks = dag.noChecks_;
		dag.noChecks_ = true;
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				ISA.getNode(dag), BINARY_PREDICATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ARGISA.getNode(dag),
				ISA.getNode(dag), PrimitiveNode.parseNode("2"),
				COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				GENLS.getNode(dag), BINARY_PREDICATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ARGISA.getNode(dag),
				GENLS.getNode(dag), PrimitiveNode.parseNode("1"),
				COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ARGISA.getNode(dag),
				GENLS.getNode(dag), PrimitiveNode.parseNode("2"),
				COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, GENLS.getNode(dag),
				BINARY_PREDICATE.getNode(dag), PREDICATE.getNode(dag));

		// Individual
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				INDIVIDUAL.getNode(dag), COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, DISJOINTWITH.getNode(dag),
				INDIVIDUAL.getNode(dag), COLLECTION.getNode(dag));

		// Collection
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				COLLECTION.getNode(dag), COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				THING.getNode(dag), COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, GENLS.getNode(dag),
				COLLECTION.getNode(dag), THING.getNode(dag));

		// Predicate
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				PREDICATE.getNode(dag), COLLECTION.getNode(dag));

		// Function
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				FUNCTION.getNode(dag), COLLECTION.getNode(dag));

		// NLP Predicates
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				BINARY_PREDICATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ARGISA.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag),
				PrimitiveNode.parseNode("1"), PREDICATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, GENLPREDS.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag), TERM_STRING.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, COMMENT.getNode(dag),
				NLP_PREDICATE_STRING.getNode(dag), new StringNode(
						"A predicate for converting binary assertions into "
								+ "a natural language phrase. ("
								+ NLP_PREDICATE_STRING.nodeName_
								+ " PRED NLPSTR) means that PRED can be "
								+ "described using NLPSTR in the context of "
								+ "an adverb (e.g. 'an instance of', 'broader "
								+ "than', etc)."));

		// Reversed NLP
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				REVERSED_NLP.getNode(dag), COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, GENLS.getNode(dag),
				REVERSED_NLP.getNode(dag), dag.findOrCreateNode("NLRelation",
						_COMMON_CONCEPT, true, true, false));
		dag.findOrCreateEdge(
				_COMMON_CONCEPT,
				false,
				COMMENT.getNode(dag),
				REVERSED_NLP.getNode(dag),
				new StringNode(
						"A collection containing binary predicates which have "
								+ "'reversed' NLP descriptions. That is, they are more easily "
								+ "explained by introducing the second arg, then the predicate, "
								+ "followed by the first argument."));

		// First Order Collection
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, GENLS.getNode(dag),
				FIRST_ORDER_COLLECTION.getNode(dag), COLLECTION.getNode(dag));

		// Primitives
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				STRING.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				POSITIVE_INTEGER.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				NEGATIVE_INTEGER.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				ZERO.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				POSITIVE_NUMBER.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				NEGATIVE_NUMBER.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				TRUE.getNode(dag), INDIVIDUAL.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				FALSE.getNode(dag), INDIVIDUAL.getNode(dag));

		// Time functions
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				DATE.getNode(dag), COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				DAYFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, RESULTISA.getNode(dag),
				DAYFN.getNode(dag), DATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				MONTHFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, RESULTISA.getNode(dag),
				MONTHFN.getNode(dag), DATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				YEARFN.getNode(dag), UNREIFIABLE_FUNCTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, RESULTISA.getNode(dag),
				YEARFN.getNode(dag), DATE.getNode(dag));

		// Temporal Concepts
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				LATER_PREDICATE.getNode(dag), BINARY_PREDICATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ARGISA.getNode(dag),
				LATER_PREDICATE.getNode(dag), PrimitiveNode.parseNode("1"),
				DATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ARGISA.getNode(dag),
				LATER_PREDICATE.getNode(dag), PrimitiveNode.parseNode("2"),
				DATE.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				INTERVAL_BOUND.getNode(dag), COLLECTION.getNode(dag));
		dag.findOrCreateEdge(_COMMON_CONCEPT, false, ISA.getNode(dag),
				INTERVAL_FUNCTION.getNode(dag),
				UNREIFIABLE_FUNCTION.getNode(dag));

		nlpPredicates(COMMENT, "described as", false, dag);
		nlpPredicates(DISJOINTWITH, "disjoint with", false, dag);
		nlpPredicates(GENLINVERSE, "the supertype of", false, dag);
		nlpPredicates(GENLMT, "a sub-microtheory of", false, dag);
		nlpPredicates(GENLPREDS, "a sub-predicate of", false, dag);
		nlpPredicates(GENLS, "a subtype of", false, dag);
		nlpPredicates(ISA, "an instance of", false, dag);
		nlpPredicates(NLP_PREDICATE_STRING, "the NL predicate for", true, dag);
		nlpPredicates(PRETTY_STRING_CANONICAL, "the preferred term for", true,
				dag);
		nlpPredicates(QUOTED_ISA, "implicitly an instance of", false, dag);
		nlpPredicates(RESULTGENL, "the type produced by", false, dag);
		nlpPredicates(RESULTISA, "the instance produced by", false, dag);
		nlpPredicates(TERM_STRING, "also known as", false, dag);
		dag.noChecks_ = checks;
	}

	public static void initialise(CycDAG dag) {
		for (CommonConcepts cc : CommonConcepts.values())
			cc.getNode(dag);
	}
}
