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
import java.util.regex.Pattern;

import util.UtilityMethods;

public class NLPToStringModule extends DAGModule<String> {
	private static final long serialVersionUID = -2277403308776894624L;
	public static final byte EDGE = 1;
	public static final byte NODE = 0;
	public static final byte QUERY = 2;
	private static final String WHAT_THINGS = "what things";

	private transient QueryModule querier_;

	private String conjunctedEdgeToString(QueryObject queryObject,
			boolean isQuery) {
		Node[] conjuncted = queryObject.getNodes();
		StringBuffer buffer = new StringBuffer();
		for (int i = 1; i < conjuncted.length; i++) {
			if (i > 1) {
				if (isQuery)
					buffer.deleteCharAt(buffer.length() - 1);
				buffer.append(" "
						+ queryObject.getNode(0).getName().toUpperCase() + " ");
			}
			QueryObject conjQO = new QueryObject(
					((OntologyFunction) conjuncted[i]).getNodes());
			String conjStr = edgeToString(conjQO, isQuery, true);
			buffer.append(conjStr);
		}
		return buffer.toString();
	}

	private void initQuerier() {
		if (querier_ == null)
			querier_ = (QueryModule) dag_.getModule(QueryModule.class);
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

	public String edgeToString(QueryObject queryObject, boolean isQuery,
			boolean includeVariables) {
		initQuerier();

		if (queryObject.getNode(0).equals(CommonConcepts.AND.getNode(dag_))
				|| queryObject.getNode(0).equals(
						CommonConcepts.OR.getNode(dag_))) {
			String conj = conjunctedEdgeToString(queryObject, isQuery);
			return conj;
		}

		// Form the replacement strings
		Node[] nodes = queryObject.getNodes();
		String[] replacements = new String[nodes.length];
		replacements[0] = "'" + nodes[0].getName() + "'";
		for (int i = 1; i < replacements.length; i++) {
			if (nodes[i] instanceof VariableNode) {
				replacements[i] = WHAT_THINGS;
				if (includeVariables)
					replacements[i] += " " + nodes[i];
			} else
				replacements[i] = nodeToString(nodes[i]);
			if (nodes[i] instanceof DAGNode)
				replacements[i] = "'" + replacements[i] + "'";
		}

		Collection<Node> predicateStrings = querier_.executeAndParseVar(
				new QueryObject(CommonConcepts.NLP_PREDICATE_STRING
						.getNode(dag_), queryObject.getNode(0),
						VariableNode.DEFAULT), "?X");
		if (predicateStrings.isEmpty()) {
			// No NLP String, revert to default
			StringBuffer buffer = new StringBuffer();
			if (isQuery && queryObject.isProof())
				buffer.append("does ");
			buffer.append(replacements[1]);
			if (isQuery
					&& (nodes[1] instanceof VariableNode || queryObject
							.isProof()))
				buffer.append(" have ");
			else
				buffer.append(" has ");
			buffer.append(replacements[0] + " relation");
			for (int i = 2; i < nodes.length; i++) {
				if (i == 2)
					buffer.append(" to ");
				else if (i == nodes.length - 1)
					buffer.append(" and ");
				else
					buffer.append(", ");
				buffer.append(replacements[i]);
			}
			if (isQuery)
				buffer.append("?");
			return buffer.toString();
		} else {
			// Replace elements in the NLP string
			String nlpString = UtilityMethods.shrinkString(predicateStrings
					.iterator().next().toString(), 1);
			String replaced = UtilityMethods.replaceToken(nlpString,
					replacements);

			// Query checking
			if (isQuery && queryObject.isProof()) {
				// Remove the 1st arg pattern
				int oldLength = replaced.length();
				replaced = replaced.replaceAll(
						" ?\\|1\\((.+?)\\)\\|\\((.+?)\\)\\| ?", " ");
				if (replaced.length() == oldLength)
					replaced = replaced.replaceFirst(
							" ?\\|\\d+\\((.+?)\\)\\|\\((.+?)\\)\\| ?", " ");
				replaced = "is " + replaced;
			}

			for (int i = 0; i < replacements.length; i++) {
				Matcher m = Pattern.compile(
						"\\|" + i + "\\((.+?)\\)\\|\\((.+?)\\)\\|").matcher(
						replaced);
				if (replacements[i].startsWith(WHAT_THINGS)) {
					replaced = m.replaceAll("$2");
					// Ensuring variable remains in
					if (!replaced.contains(replacements[i]))
						replaced = m.replaceAll("$2 " + nodes[i]);
				} else
					replaced = m.replaceAll("$1");
			}

			if (isQuery)
				replaced += "?";
			return replaced;
		}
	}

	@Override
	public String execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (args == null)
			return null;
		if (args.length > 1 && args instanceof Node[])
			return edgeToString(new QueryObject((Node[]) args), false, false);
		else if (args[0] instanceof Node)
			return nodeToString((Node) args[0]);
		else if (args[0] instanceof Edge)
			return edgeToString(new QueryObject(((Edge) args[0]).getNodes()),
					false, false);
		else if (args[0] instanceof QueryObject)
			return edgeToString((QueryObject) args[0], true, false);
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
