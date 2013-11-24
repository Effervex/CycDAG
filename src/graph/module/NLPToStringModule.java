package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.VariableNode;

import java.util.Collection;
import java.util.regex.Matcher;

import util.UtilityMethods;

public class NLPToStringModule extends DAGModule<String> {
	private static final long serialVersionUID = -2277403308776894624L;
	public static final byte EDGE = 1;
	public static final byte NODE = 0;
	public static final byte QUERY = 2;

	private transient QueryModule querier_;

	private String conjunctedEdgeToString(QueryObject queryObject,
			boolean isQuery) {
		Node[] conjuncted = queryObject.getNodes();
		StringBuffer buffer = new StringBuffer();
		for (int i = 1; i < conjuncted.length; i++) {
			if (i > 1) {
				buffer.deleteCharAt(buffer.length() - 1);
				buffer.append(" "
						+ queryObject.getNode(0).getName().toUpperCase() + " ");
			}
			QueryObject conjQO = new QueryObject(
					((OntologyFunction) conjuncted[i]).getNodes());
			String conjStr = edgeToString(conjQO, isQuery);
			// Insert variables for clarification of language
			if (conjQO.getVariableIndex() != -1)
				conjStr = conjStr.replaceAll("what things", "what things "
						+ conjQO.getVariable(conjQO.getVariableIndex() - 1));
			buffer.append(conjStr);
		}
		return buffer.toString();
	}

	private void initQuerier() {
		if (querier_ == null)
			querier_ = (QueryModule) dag_.getModule(QueryModule.class);
	}

	private String proofEdgeToString(boolean isQuery, boolean hasNLPString,
			String predicateString, boolean reversed, String firstArg,
			String secondArg) {
		// Proof queries
		if (hasNLPString) {
			if (reversed) {
				String temp = firstArg;
				firstArg = secondArg;
				secondArg = temp;
			}

			if (isQuery)
				return "is " + firstArg + " " + predicateString + " "
						+ secondArg + "?";
			else
				return firstArg + " is " + predicateString + " " + secondArg;
		} else {
			if (isQuery)
				return "does " + firstArg + " have " + predicateString + " "
						+ secondArg + "?";
			else
				return firstArg + " has " + predicateString + " " + secondArg;
		}
	}

	private String queryEdgeToString(QueryObject queryObject,
			boolean hasNLPString, String predicateString, boolean reversed,
			String firstArg, String secondArg) {
		// Substitution queries
		int varIndex = queryObject.getVariableIndex();

		if (hasNLPString) {
			String nonVar = (varIndex == 1) ? secondArg : firstArg;
			if ((varIndex == 1 && !reversed) || (varIndex == 2 && reversed))
				return "what things are " + predicateString + " " + nonVar
						+ "?";
			else
				return nonVar + " is " + predicateString + " what things?";
		} else {
			if (varIndex == 1)
				return "what things have " + predicateString + " " + secondArg
						+ "?";
			else
				return firstArg + " has " + predicateString + " what things?";
		}
	}

	private String returnSmallestString(Collection<? extends Object> objs) {
		String smallest = null;
		for (Object n : objs) {
			String ns = n.toString();
			if (smallest == null || ns.length() < smallest.length())
				smallest = ns;
		}
		return smallest;
	}

	public String edgeToString(QueryObject queryObject, boolean isQuery) {
		if (queryObject.getNodes().length < 3)
			return "Cannot process unary predicates: " + queryObject.toString();
		initQuerier();

		if (queryObject.getNode(0).equals(CommonConcepts.AND.getNode(dag_))
				|| queryObject.getNode(0).equals(
						CommonConcepts.OR.getNode(dag_))) {
			return conjunctedEdgeToString(queryObject, isQuery);
		}

		// Check for NLP string
		Collection<Node> predicateStrings = querier_.executeAndParseVar(
				new QueryObject(CommonConcepts.NLP_PREDICATE_STRING
						.getNode(dag_), queryObject.getNode(0),
						VariableNode.DEFAULT), "?X");
		boolean hasNLPString = !predicateStrings.isEmpty();
		String predicateString = (!hasNLPString) ? "'"
				+ queryObject.getNode(0).getName() + "' relation to"
				: UtilityMethods.shrinkString(predicateStrings.iterator()
						.next().toString(), 1);

		// Determine if reversed
		boolean reversed = querier_.prove(
				CommonConcepts.QUOTED_ISA.getNode(dag_),
				queryObject.getNode(0),
				CommonConcepts.REVERSED_NLP.getNode(dag_));
		// Get node names
		String firstArg = nodeToString(queryObject.getNode(1));
		if (queryObject.getNode(1) instanceof DAGNode)
			firstArg = "'" + firstArg + "'";

		StringBuffer secondArg = new StringBuffer();
		for (int i = 2; i < queryObject.getNodes().length; i++) {
			if (secondArg.length() != 0)
				secondArg.append(" and ");
			Node n = queryObject.getNode(i);
			String arg = nodeToString(n);
			if (n instanceof DAGNode)
				arg = "'" + arg + "'";
			secondArg.append(arg);
		}

		if (queryObject.isProof()) {
			return proofEdgeToString(isQuery, hasNLPString, predicateString,
					reversed, firstArg, secondArg.toString());
		} else {
			return queryEdgeToString(queryObject, hasNLPString,
					predicateString, reversed, firstArg, secondArg.toString());
		}
	}

	@Override
	public String execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (args == null)
			return null;
		if (args.length > 1 && args instanceof Node[])
			return edgeToString(new QueryObject((Node[]) args), false);
		else if (args[0] instanceof Node)
			return nodeToString((Node) args[0]);
		else if (args[0] instanceof Edge)
			return edgeToString(new QueryObject(((Edge) args[0]).getNodes()),
					false);
		else if (args[0] instanceof QueryObject)
			return edgeToString((QueryObject) args[0], true);
		return null;
	}

	public String nodeToString(Node node) {
		if (!(node instanceof DAGNode))
			return node.toString();

		// Hierarchy
		initQuerier();

		// Use prettyString-Canonical where possible
		Collection<Node> canonicalStrs = querier_.executeAndParseVar(
				new QueryObject(CommonConcepts.PRETTY_STRING_CANONICAL
						.getNode(dag_), node, VariableNode.DEFAULT), "?X");
		if (!canonicalStrs.isEmpty())
			return UtilityMethods.shrinkString(
					returnSmallestString(canonicalStrs), 1);

		// Use any termStrings
		Collection<Node> termStrings = querier_.executeAndParseVar(
				new QueryObject(CommonConcepts.TERM_STRING.getNode(dag_), node,
						VariableNode.DEFAULT), "?X");
		if (!termStrings.isEmpty())
			return UtilityMethods.shrinkString(
					returnSmallestString(termStrings), 1);

		// Convert name into plain text
		if (node instanceof OntologyFunction) {
			Node[] args = ((OntologyFunction) node).getNodes();
			StringBuffer argString = new StringBuffer();
			for (int i = 1; i < args.length; i++) {
				if (argString.length() != 0)
					argString.append(" & ");
				argString.append(nodeToString(args[i]));
			}

			// Function string
			String function = nodeToString(args[0]);
			if (function.contains("___")) {
				return function.replaceAll("___+",
						Matcher.quoteReplacement(argString.toString()));
			} else {
				function = function.replaceAll(" Fn$", "");
				function = argString.toString() + " (" + function + ")";
				return function.replaceAll("\\) \\(", ", ");
			}
		} else {
			return conceptToPlainText(node.getName());
		}
	}

	// TODO Refactor this method
	public static String conceptToPlainText(String concept) {
		String result = "";
		// Splitting hyphens into brackets.
		if (concept.contains("-") && !concept.endsWith("-")) {
			String[] elements = concept.split("-");
			concept = elements[0];
			int i = 1;
			for (i = 1; i < elements.length - 1; i++) {
				concept += "-" + elements[i];
			}
			concept += " (" + elements[i] + ")";
		}

		// Checking through, char by char
		for (int i = 0; i < concept.length(); i++) {
			char c = concept.charAt(i);

			// is it an upper case character?
			if (Character.isUpperCase(c) || Character.isDigit(c)) {
				// if yes, is this NOT the start of the string?
				// or beginning of the bracket expression?
				if (!result.equals("") && !result.endsWith("(")) {
					// we are in the middle of a phrase..
					// Is the previous character not a space?
					if (concept.charAt(i - 1) != ' ') {
						// is it an A (not part of abbreviation)?
						if ((c == 'A')
								&& Character.isLowerCase(concept.charAt(i - 1))) {

							result = result + " ";
						} else if (Character.isLowerCase(concept.charAt(i - 1))) {
							// is the letter before a lower case character?
							result = result + " ";
						} else if (i < concept.length() - 1
								&& Character.isLowerCase(concept.charAt(i + 1))
								&& !Character.isDigit(c)) {

							// is the letter before an upper case character?
							result = result + " ";
						}

					}
				}
			}
			result = result + c;
		}
		return result;
	}
}
