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
package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.Edge;
import graph.core.Node;
import graph.core.OntologyFunction;
import graph.inference.QueryObject;
import graph.inference.Substitution;
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
	private static final Pattern MARKUP_PATTERN = Pattern
			.compile("\\[\\[(.+?)(?:\\|.+?)?\\]\\]");

	private transient QueryModule querier_;

	private String conjunctedEdgeToString(QueryObject queryObject,
			boolean isQuery, boolean markup) {
		Node[] conjuncted = queryObject.getNodes();
		StringBuilder buffer = new StringBuilder();
		for (int i = 1; i < conjuncted.length; i++) {
			if (i > 1) {
				if (isQuery)
					buffer.deleteCharAt(buffer.length() - 1);
				buffer.append(" "
						+ queryObject.getNode(0).getName().toUpperCase() + " ");
			}
			QueryObject conjQO = new QueryObject(
					((OntologyFunction) conjuncted[i]).getNodes());
			String conjStr = edgeToString(conjQO, isQuery, true, markup, false);
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
			boolean includeVariables, boolean markup, boolean negated) {
		initQuerier();

		// Dealing with negation
		if (queryObject.getNode(0).equals(CommonConcepts.NOT.getNode(dag_))) {
			QueryObject innerQO = new QueryObject(
					((OntologyFunction) queryObject.getNodes()[1]).getNodes());
			return edgeToString(innerQO, isQuery, includeVariables, markup,
					!negated);
		}

		// Dealing with conjunctions
		if (queryObject.getNode(0).equals(CommonConcepts.AND.getNode(dag_))
				|| queryObject.getNode(0).equals(
						CommonConcepts.OR.getNode(dag_))) {
			String conj = conjunctedEdgeToString(queryObject, isQuery, markup);
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
			} else {
				replacements[i] = nodeToString(nodes[i], markup);
			}
		}

		// Get the predicate string if it exists.
		Collection<Substitution> predicateStrings = querier_
				.execute(new QueryObject(CommonConcepts.NLP_PREDICATE_STRING
						.getNode(dag_), queryObject.getNode(0),
						VariableNode.DEFAULT));
		if (predicateStrings.isEmpty()) {
			// No NLP String, revert to default
			StringBuilder buffer = new StringBuilder();
			if (isQuery && queryObject.isProof())
				buffer.append("does ");
			buffer.append(replacements[1]);
			if (negated) {
				if (!isQuery)
					buffer.append(" does");
				buffer.append(" not");
			}
			if (negated
					|| (isQuery && (nodes[1] instanceof VariableNode || queryObject
							.isProof())))
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
			String replaced = UtilityMethods.shrinkString(predicateStrings
					.iterator().next().getSubstitution(VariableNode.DEFAULT)
					.toString(), 1);

			// Negation
			if (negated) {
				replaced = negateString(replaced);
			}

			// Query checking
			if (isQuery && queryObject.isProof()) {
				// Remove the 1st arg pattern
				replaced = replaced.replaceFirst(
						" ?/\\d+\\((.+?)\\)/\\((.+?)\\)/ ?", " ");
				replaced = "is " + replaced;
			}

			for (int i = 0; i < replacements.length; i++) {
				Matcher m = Pattern.compile(
						"/" + i + "\\((.+?)\\)/\\((.+?)\\)/").matcher(replaced);
				if (replacements[i].startsWith(WHAT_THINGS))
					replaced = m.replaceAll("$2");
				else
					replaced = m.replaceAll("$1");
			}

			// Insert the arguments
			replaced = UtilityMethods.replaceToken(replaced, replacements);

			if (isQuery)
				replaced += "?";
			return replaced;
		}
	}

	/**
	 * Negate a predicate string by replacing a token with a negated token, or
	 * simply appending 'is not true' to the end of the string.
	 *
	 * @param replaced
	 *            The string to negate.
	 * @return A negated form of the string.
	 */
	private String negateString(String target) {
		if (target.contains("(is)/(are)/"))
			return target.replaceAll("\\(is\\)/\\(are\\)/", "$0 not");
		if (target.contains("(has)/(have)/"))
			return target.replaceAll("\\(has\\)/\\(have\\)/",
					"(does not have)/(do not have)/");
		if (target.contains(" is "))
			return target.replaceFirst(" is ", "$0not ");
		return target + " is not true";
	}

	@Override
	public String execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (args == null)
			return null;

		// Check for markup boolean
		boolean markup = false;
		int i = 0;
		if (args[0] instanceof Boolean) {
			markup = (boolean) args[0];
			i++;
		}

		// Parse and execute the object
		if (args[i] instanceof Node[]) {
			return edgeToString(new QueryObject((Node[]) args[i]), false,
					false, markup, false);
		} else if (args[i] instanceof Node)
			return nodeToString((Node) args[i], markup);
		else if (args[i] instanceof Edge)
			return edgeToString(new QueryObject(((Edge) args[i]).getNodes()),
					false, false, markup, false);
		else if (args[i] instanceof QueryObject)
			return edgeToString((QueryObject) args[i], true, false, markup,
					false);
		else if (args[i] instanceof String)
			return markupToString((String) args[i], markup);
		return null;
	}

	@Override
	public boolean addNode(DAGNode node) {
		// Add its alias to the NodeAlias map
		NodeAliasModule nam = (NodeAliasModule) dag_
				.getModule(NodeAliasModule.class);
		if (nam != null) {
			String nodeName = nodeToString(node, false);
			if (!nodeName.isEmpty())
				nam.addAlias(node, nodeName);
			if (!node.getName().startsWith("(")) {
				String basicName = conceptToPlainText(node.getName());
				if (!basicName.equals(nodeName)
						&& !basicName.equals(node.getName())
						&& !basicName.isEmpty())
					nam.addAlias(node, basicName);
			}
		}
		return super.addNode(node);
	}

	@Override
	public boolean removeNode(DAGNode node) {
		// Remove the node alias
		NodeAliasModule nam = (NodeAliasModule) dag_
				.getModule(NodeAliasModule.class);
		if (nam != null) {
			String nodeName = nodeToString(node, false);
			if (!nodeName.isEmpty())
				nam.removeAlias(node, nodeName);
			if (!node.getName().startsWith("(")) {
				String basicName = conceptToPlainText(node.getName());
				if (!basicName.equals(nodeName)
						&& !basicName.equals(node.getName())
						&& !basicName.isEmpty())
					nam.removeAlias(node, basicName);
			}
		}
		return super.addNode(node);
	}

	public String markupToString(String string, boolean markup) {
		// Find all instances of [[text]] and swap it for dag-to-text, OR append
		// it with dag-to-text if markup is active.
		StringBuilder buffer = new StringBuilder();
		int start = 0;
		Matcher m = MARKUP_PATTERN.matcher(string);
		while (m.find()) {
			int index = m.start();
			buffer.append(string.subSequence(start, index));
			if (markup)
				buffer.append("[[" + m.group(1) + "|");
			Node node = dag_.findOrCreateNode(m.group(1), null, false, false,
					true, false);
			if (node == null)
				buffer.append(m.group(1));
			else
				buffer.append(nodeToString(node, false));
			if (markup)
				buffer.append("]]");
			start = m.end();
		}
		buffer.append(string.substring(start));
		return buffer.toString();
	}

	private String nodeToStringInternal(Node node) {
		// Use prettyString-Canonical where possible
		Collection<Node> canonicalStrs = querier_.executeAndParseVar(
				new QueryObject(CommonConcepts.ASSERTED_SENTENCE.getNode(dag_),
						new OntologyFunction(
								CommonConcepts.PRETTY_STRING_CANONICAL
										.getNode(dag_), node,
								VariableNode.DEFAULT)), VariableNode.DEFAULT
						.toString());
		if (!canonicalStrs.isEmpty())
			return UtilityMethods.shrinkString(
					returnSmallestString(canonicalStrs), 1);

		// Use children of prettyString-Canonical
		canonicalStrs = querier_.executeAndParseVar(new QueryObject(
				CommonConcepts.PRETTY_STRING_CANONICAL.getNode(dag_), node,
				VariableNode.DEFAULT), VariableNode.DEFAULT.toString());
		if (!canonicalStrs.isEmpty())
			return UtilityMethods.shrinkString(
					returnSmallestString(canonicalStrs), 1);

		// Use any termStrings
		Collection<Node> termStrings = querier_.executeAndParseVar(
				new QueryObject(CommonConcepts.TERM_STRING.getNode(dag_), node,
						VariableNode.DEFAULT), VariableNode.DEFAULT.toString());
		if (!termStrings.isEmpty())
			return UtilityMethods.shrinkString(
					returnSmallestString(termStrings), 1);

		// Convert name into plain text
		if (node instanceof OntologyFunction) {
			Node[] args = ((OntologyFunction) node).getNodes();
			StringBuilder argString = new StringBuilder();
			for (int i = 1; i < args.length; i++) {
				if (argString.length() != 0)
					argString.append(" & ");
				argString.append(nodeToStringInternal(args[i]));
			}

			// Function string
			String function = nodeToStringInternal(args[0]);
			if (function.contains("___")) {
				return function.replaceAll("___+",
						Matcher.quoteReplacement(argString.toString()));
			} else {
				function = function.replaceAll(" Fn$", "");
				function = argString.toString() + " (" + function + ")";
				return function.replaceAll("\\) \\(", ", ");
			}
		} else {
			if (node instanceof DAGNode)
				return conceptToPlainText(node.getName());
			return node.getName();
		}
	}

	public String nodeToString(Node node, boolean markup) {
		if (!(node instanceof DAGNode))
			return node.toString();

		// Hierarchy
		initQuerier();
		String result = nodeToStringInternal(node);
		if (markup && !(result.startsWith("<") && result.endsWith(">")))
			return "[[" + node.toString() + "|" + result + "]]";
		return result;
	}

	// TODO Refactor this method
	public static String conceptToPlainText(String concept) {
		String result = "";
		concept = concept.replaceAll("_", " ");
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
