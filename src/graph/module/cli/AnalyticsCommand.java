package graph.module.cli;

import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.cli.DAGPortHandler;
import graph.inference.CommonQuery;
import graph.inference.QueryObject;
import graph.inference.QueryResult;
import graph.inference.Substitution;
import graph.inference.VariableNode;
import graph.module.FunctionIndex;
import graph.module.QueryModule;
import graph.module.RelatedEdgeModule;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import util.UtilityMethods;
import core.Command;

public class AnalyticsCommand extends Command {
	private static final String MEMBERS = "# Members";
	private static final String MAX_SUBTYPES = "# Max Subtypes";
	private static final String MIN_SUPERTYPES = "# Min Supertypes";
	private static final String COMMENTED = "Commented?";
	private static final String PRETTY_STRING = "Pretty String?";
	private static final String ARG_CONSTRAINTS = "Arg Constraint(s)?";
	private static final String ARITY = "Arity";
	private static final String SUBTYPES = "# Subtypes";
	private static final String SUPERTYPES = "# Supertypes";
	private static final String RELATION_USES = "# Uses";
	private static final String NAME = "Name";
	private static final String ID = "ID";
	private static final String RESULT_CONSTRAINTS = "Result Constraint(s)?";
	private static final String REIFIABLE = "Reifiable?";
	private static final String DIRECT_ISA = "# Direct Isa";
	private QueryModule querier_;
	private RelatedEdgeModule relEdgeModule_;
	private FunctionIndex funcIndex_;

	private enum AnalyticTypes {
		COLLECTIONS, PREDICATES, FUNCTIONS, INDIVIDUALS;
	}

	// TODO Direct Links
	public static final String[] FUNCTION_FIELDS = { ID, NAME, RELATION_USES,
			ARITY, REIFIABLE, RESULT_CONSTRAINTS, ARG_CONSTRAINTS,
			PRETTY_STRING, COMMENTED };
	private static final String[] COLLECTION_FIELDS = { ID, NAME, MEMBERS,
			SUPERTYPES, SUBTYPES, MIN_SUPERTYPES, MAX_SUBTYPES, PRETTY_STRING,
			COMMENTED };
	private static final String[] RELATION_FIELDS = { ID, NAME, RELATION_USES,
			SUPERTYPES, SUBTYPES, ARITY, ARG_CONSTRAINTS, PRETTY_STRING,
			COMMENTED };
	private static final String[] INDIVIDUAL_FIELDS = { ID, NAME, DIRECT_ISA,
			MIN_SUPERTYPES, PRETTY_STRING, COMMENTED };

	@Override
	public String helpText() {

		return "{0} file TYPE | Prints out analytics of a specific "
				+ "type about the DAG to file. Types include: "
				+ StringUtils.join(AnalyticTypes.values(), ',') + ".";
	}

	@Override
	public String shortDescription() {
		return "Prints a number of analytics of the DAG to file.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		CycDAG dag = (CycDAG) dagHandler.getDAG();
		querier_ = (QueryModule) dag.getModule(QueryModule.class);
		if (querier_ == null) {
			print("-1|Query Module is not in use for this DAG.\n");
			return;
		}
		relEdgeModule_ = (RelatedEdgeModule) dag
				.getModule(RelatedEdgeModule.class);
		if (relEdgeModule_ == null) {
			print("-1|Related Edge Module is not in use for this DAG.\n");
			return;
		}
		funcIndex_ = (FunctionIndex) dag.getModule(FunctionIndex.class);
		if (funcIndex_ == null) {
			print("-1|Function Index Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}
		String[] split = UtilityMethods.splitToArray(data, ' ');
		if (split.length != 2) {
			print("-1|Error parsing input. Please provide a filename and analytic type.\n");
			return;
		}

		try {
			AnalyticTypes type = AnalyticTypes.valueOf(split[1]);
			if (type == null) {
				print("-1|Could not parse type. Please use: "
						+ StringUtils.join(AnalyticTypes.values(), ',') + ".\n");
				return;
			}

			File outputFile = new File(split[0].trim());
			outputFile.createNewFile();
			BufferedWriter out = new BufferedWriter(new FileWriter(outputFile));

			switch (type) {
			case COLLECTIONS:
				Collection<Substitution> collections = querier_.executeQuery(
						false,
						new QueryObject(CommonConcepts.ISA.getNode(dag),
								VariableNode.DEFAULT, CommonConcepts.COLLECTION
										.getNode(dag)));
				processNodes(collections, COLLECTION_FIELDS, type, dag, out);
				break;
			case PREDICATES:
				Collection<Substitution> predicates = querier_.executeQuery(
						false,
						new QueryObject(CommonConcepts.ISA.getNode(dag),
								VariableNode.DEFAULT, CommonConcepts.PREDICATE
										.getNode(dag)));
				processNodes(predicates, RELATION_FIELDS, type, dag, out);
				break;
			case FUNCTIONS:
				Collection<Substitution> functions = querier_.executeQuery(
						false,
						new QueryObject(CommonConcepts.ISA.getNode(dag),
								VariableNode.DEFAULT, CommonConcepts.FUNCTION
										.getNode(dag)));
				processNodes(functions, FUNCTION_FIELDS, type, dag, out);
				break;
			case INDIVIDUALS:
				Collection<Substitution> individuals = querier_.executeQuery(
						false,
						new QueryObject(CommonConcepts.ISA.getNode(dag),
								VariableNode.DEFAULT, CommonConcepts.INDIVIDUAL
										.getNode(dag)));
				processNodes(individuals, INDIVIDUAL_FIELDS, type, dag, out);
				break;
			}

			out.close();
		} catch (Exception e) {
			print("-13|Error during analysis!\n" + e.getMessage() + "\n");
		}
	}

	private void processNodes(Collection<Substitution> nodeCollection,
			String[] fields, AnalyticTypes type, CycDAG dag, BufferedWriter out)
			throws IOException {
		// Write header
		Map<String, Integer> indexMap = new HashMap<>();
		for (int i = 0; i < fields.length; i++)
			indexMap.put(fields[i], i);
		out.write(StringUtils.join(fields, '\t') + "\n");
		int total = nodeCollection.size();
		double frac = 0.1;
		int count = 0;
		for (Substitution sub : nodeCollection) {
			Node node = sub.getSubstitution(VariableNode.DEFAULT);
			String[] values = new String[fields.length];

			if (type == AnalyticTypes.COLLECTIONS)
				processCollection(node, indexMap, values, dag);
			else if (type == AnalyticTypes.FUNCTIONS)
				processFunction(node, indexMap, values, dag);
			else if (type == AnalyticTypes.PREDICATES)
				processPredicate(node, indexMap, values, dag);
			else if (type == AnalyticTypes.INDIVIDUALS)
				processIndividual(node, indexMap, values, dag);

			out.write(StringUtils.join(values, '\t') + "\n");
			count++;
			if (count >= frac * total) {
				print((int) Math.round(frac * 100) + "% ");
				flushOut();
				frac += 0.1;
			}
		}
		print("\n");
	}

	private void processFunction(Node function, Map<String, Integer> indexMap,
			String[] values, CycDAG dag) {
		// ID and name
		values[indexMap.get(ID)] = function.getIdentifier();
		values[indexMap.get(NAME)] = function.getName();

		// Num uses
		values[indexMap.get(RELATION_USES)] = funcIndex_
				.getInstantiatedFunctionConcepts((DAGNode) function).size()
				+ "";

		// Arity
		Collection<Substitution> arity = querier_.executeQuery(false,
				new QueryObject(CommonConcepts.ARITY.getNode(dag), function,
						VariableNode.DEFAULT));
		values[indexMap.get(ARITY)] = (arity.isEmpty()) ? "" : arity.iterator()
				.next() + "";

		// Reifiable?
		values[indexMap.get(REIFIABLE)] = (querier_.prove(false,
				CommonConcepts.ISA.getNode(dag), function,
				CommonConcepts.UNREIFIABLE_FUNCTION.getNode(dag)) == QueryResult.TRUE) ? "F"
				: "T";

		// Result constraints?
		boolean resultConstraint = querier_.prove(false,
				CommonConcepts.RESULT_ISA.getNode(dag), function,
				VariableNode.DEFAULT) == QueryResult.TRUE;
		resultConstraint |= querier_.prove(false,
				CommonConcepts.RESULT_GENL.getNode(dag), function,
				VariableNode.DEFAULT) == QueryResult.TRUE;
		resultConstraint |= querier_.prove(false,
				CommonConcepts.RESULT_ISA_ARG.getNode(dag), function,
				VariableNode.DEFAULT) == QueryResult.TRUE;
		resultConstraint |= querier_.prove(false,
				CommonConcepts.RESULT_GENL_ARG.getNode(dag), function,
				VariableNode.DEFAULT) == QueryResult.TRUE;
		values[indexMap.get(RESULT_CONSTRAINTS)] = (resultConstraint) ? "T"
				: "F";

		// If Arg constraints
		boolean argConstraint = !relEdgeModule_.execute(
				CommonConcepts.ARGISA.getNode(dag), 1, function, 2).isEmpty();
		argConstraint |= !relEdgeModule_.execute(
				CommonConcepts.ARGGENL.getNode(dag), 1, function, 2).isEmpty();
		values[indexMap.get(ARG_CONSTRAINTS)] = (argConstraint) ? "T" : "F";

		// If pretty strings
		values[indexMap.get(PRETTY_STRING)] = (CommonQuery.ALIAS.runQuery(dag,
				function).isEmpty()) ? "F" : "T";

		// If comment
		values[indexMap.get(COMMENTED)] = (CommonQuery.COMMENT.runQuery(dag,
				function).isEmpty()) ? "F" : "T";
	}

	private void processPredicate(Node predicate,
			Map<String, Integer> indexMap, String[] values, CycDAG dag) {
		// ID and name
		values[indexMap.get(ID)] = predicate.getIdentifier();
		values[indexMap.get(NAME)] = predicate.getName();

		// Num uses
		values[indexMap.get(RELATION_USES)] = relEdgeModule_.execute(predicate,
				1).size()
				+ "";

		// Num super/subtypes
		Collection<Node> superTypes = CommonQuery.GENLPREDS.runQuery(dag,
				predicate);
		superTypes.remove(predicate);
		values[indexMap.get(SUPERTYPES)] = superTypes.size() + "";
		Collection<Node> subTypes = CommonQuery.SPECPREDS.runQuery(dag,
				predicate);
		subTypes.remove(predicate);
		values[indexMap.get(SUBTYPES)] = subTypes.size() + "";

		// Arity
		Collection<Substitution> arity = querier_.executeQuery(false,
				new QueryObject(CommonConcepts.ARITY.getNode(dag), predicate,
						VariableNode.DEFAULT));
		values[indexMap.get(ARITY)] = (arity.isEmpty()) ? "" : arity.iterator()
				.next() + "";

		// If Arg constraints
		boolean argConstraint = !relEdgeModule_.execute(
				CommonConcepts.ARGISA.getNode(dag), 1, predicate, 2).isEmpty();
		argConstraint |= !relEdgeModule_.execute(
				CommonConcepts.ARGGENL.getNode(dag), 1, predicate, 2).isEmpty();
		values[indexMap.get(ARG_CONSTRAINTS)] = (argConstraint) ? "T" : "F";

		// If pretty strings
		values[indexMap.get(PRETTY_STRING)] = (CommonQuery.ALIAS.runQuery(dag,
				predicate).isEmpty()) ? "F" : "T";

		// If comment
		values[indexMap.get(COMMENTED)] = (CommonQuery.COMMENT.runQuery(dag,
				predicate).isEmpty()) ? "F" : "T";
	}

	private void processCollection(Node collection,
			Map<String, Integer> indexMap, String[] values, CycDAG dag) {
		// ID and name
		values[indexMap.get(ID)] = collection.getIdentifier();
		values[indexMap.get(NAME)] = collection.getName();

		// Members
		Collection<Node> instances = CommonQuery.INSTANCES.runQuery(dag,
				collection);
		values[indexMap.get(MEMBERS)] = instances.size() + "";

		// Num super/subtypes
		Collection<Node> superTypes = CommonQuery.ALLGENLS.runQuery(dag,
				collection);
		superTypes.remove(collection);
		values[indexMap.get(SUPERTYPES)] = superTypes.size() + "";
		Collection<Node> subTypes = CommonQuery.SPECS.runQuery(dag, collection);
		subTypes.remove(collection);
		values[indexMap.get(SUBTYPES)] = subTypes.size() + "";

		// Min super/Max sub
		Collection<Node> minSuperTypes = CommonQuery.MINGENLS.runQuery(dag,
				collection);
		values[indexMap.get(MIN_SUPERTYPES)] = minSuperTypes.size() + "";
		Collection<Node> maxSubTypes = CommonQuery.MAXSPECS.runQuery(dag,
				collection);
		values[indexMap.get(MAX_SUBTYPES)] = maxSubTypes.size() + "";

		// If pretty strings
		values[indexMap.get(PRETTY_STRING)] = (CommonQuery.ALIAS.runQuery(dag,
				collection).isEmpty()) ? "F" : "T";

		// If comment
		values[indexMap.get(COMMENTED)] = (CommonQuery.COMMENT.runQuery(dag,
				collection).isEmpty()) ? "F" : "T";
	}

	private void processIndividual(Node individual,
			Map<String, Integer> indexMap, String[] values, CycDAG dag) {
		// ID and name
		values[indexMap.get(ID)] = individual.getIdentifier();
		values[indexMap.get(NAME)] = individual.getName();

		// Direct & min isa
		Collection<Node> superTypes = CommonQuery.DIRECTISA.runQuery(dag,
				individual);
		values[indexMap.get(DIRECT_ISA)] = superTypes.size() + "";
		Collection<Node> minSuperTypes = CommonQuery.MINISA.runQuery(dag,
				individual);
		values[indexMap.get(MIN_SUPERTYPES)] = minSuperTypes.size() + "";

		// If pretty strings
		values[indexMap.get(PRETTY_STRING)] = (CommonQuery.ALIAS.runQuery(dag,
				individual).isEmpty()) ? "F" : "T";

		// If comment
		values[indexMap.get(COMMENTED)] = (CommonQuery.COMMENT.runQuery(dag,
				individual).isEmpty()) ? "F" : "T";
	}
}
