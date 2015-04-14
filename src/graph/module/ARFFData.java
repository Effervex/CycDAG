package graph.module;

public enum ARFFData {
	RELATION("nominal"),
	ARG1("string"),
	ARG2("string"),
	DISAMB1("string"),
	DISAMB2("string"),
	SEMANTIC_SIMILARITY("numeric"),
	DEPTH_1("numeric"),
	DEPTH_2("numeric"),
	AV_DEPTH("numeric"),
	IMPACT_A("numeric"),
	IMPACT_B("numeric"),
	IMPACT_AB("numeric"),
	COMMON_PARENT("nominal"),
	PARENT_DEPTH("numeric"),
	SEMANTIC_DISTANCE("numeric"),
	RELATION_RELIABILITY("numeric"),
	RELATION_DISJOINT("numeric"),
	RELATION_CONJOINT("numeric"),
	RELATION_UNKNOWN("numeric"),
	INTRA_RELATION_RELIABILITY("numeric"),
	INTRA_RELATION_DISJOINT("numeric"),
	INTRA_RELATION_CONJOINT("numeric"),
	INTRA_RELATION_UNKNOWN("numeric"),
	CLASS("{ disjoint, conjoint }");

	private String type_;

	private ARFFData(String datatype) {
		type_ = datatype;
	}

	public String getType() {
		return type_;
	}
}
