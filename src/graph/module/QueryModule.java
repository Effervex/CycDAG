package graph.module;

import graph.core.DirectedAcyclicGraph;
import graph.core.Node;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.inference.module.AndWorker;
import graph.inference.module.AssertedSentenceWorker;
import graph.inference.module.DifferentWorker;
import graph.inference.module.DisjointWithWorker;
import graph.inference.module.EqualsWorker;
import graph.inference.module.GenlPredTransitiveWorker;
import graph.inference.module.IsaWorker;
import graph.inference.module.LaterThanWorker;
import graph.inference.module.OrWorker;
import graph.inference.module.ResultFnWorker;
import graph.inference.module.TransitiveWorker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class QueryModule extends DAGModule<Collection<Substitution>> {
	private static final long serialVersionUID = 1925714026500662430L;

	private static final String DEFAULT_WORKER = "_DEFAULT_";

	private transient Map<String, QueryWorker> inferenceModules_;
	private transient Map<String, String> regexRedirects_;

	// private BackwardChainer backwardChainer_;

	public QueryModule() {
		initInferenceModules();
	}

	private void initInferenceModules() {
		if (inferenceModules_ != null)
			return;
		inferenceModules_ = new HashMap<>();
		inferenceModules_.put("assertedSentence", new AssertedSentenceWorker(
				this));
		inferenceModules_.put("genls", new TransitiveWorker(this));
		inferenceModules_.put("genlPreds", new TransitiveWorker(this));
		inferenceModules_.put("equals", new EqualsWorker(this));
		inferenceModules_.put("different", new DifferentWorker(this));
		inferenceModules_.put("isa", new IsaWorker(this));
		inferenceModules_.put("disjointWith", new DisjointWithWorker(this));
		inferenceModules_.put("and", new AndWorker(this));
		inferenceModules_.put("or", new OrWorker(this));
		inferenceModules_.put("resultIsa", new ResultFnWorker(this));
		inferenceModules_.put("resultGenl", new ResultFnWorker(this));
		inferenceModules_.put("laterThan", new LaterThanWorker(this));
		inferenceModules_.put(DEFAULT_WORKER,
				new GenlPredTransitiveWorker(this));

		regexRedirects_ = new HashMap<>();
	}

	public void applyModule(String moduleName, QueryObject queryObj)
			throws IllegalArgumentException {
		initInferenceModules();
		inferenceModules_.get(moduleName).queryInternal(queryObj);
	}

	/**
	 * Takes a set of Nodes and Strings (in the form ?X) as arguments. Returns
	 * all variable replacements that are valid.
	 */
	@Override
	public Collection<Substitution> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		Node[] asNodes = new Node[args.length];
		System.arraycopy(args, 0, asNodes, 0, args.length);

		return execute(new QueryObject(asNodes));
	}

	public Collection<Substitution> execute(QueryObject queryObj) {
		String module = DEFAULT_WORKER;
		if (inferenceModules_.containsKey(queryObj.getNode(0).toString()))
			module = queryObj.getNode(0).getName();
		else {
			// Try the regex's
			for (String regex : regexRedirects_.keySet()) {
				if (queryObj.getNode(0).toString().matches(regex)) {
					module = regexRedirects_.get(regex);
					break;
				}
			}
		}

		// Dealing with multiple variables
		Collection<Substitution> priorSubs = queryObj.getPriorSubstitutions();
		if (queryObj.getNumVariables() > 1 && priorSubs != null
				&& !priorSubs.isEmpty()) {
			// Iterate through prior variable matches (in toComplete) to
			// minimise query to 1 or fewer variables.
			Node[] nodes = queryObj.getNodes();
			for (Substitution variableMatch : priorSubs) {
				QueryObject instantiated = queryObj.modifyNodes(variableMatch,
						variableMatch.applySubstitution(nodes));
				applyModule(module, instantiated);
			}

			return queryObj.getResults();
		} else {
			applyModule(module, queryObj);
			return queryObj.getResults();
		}
	}

	public boolean prove(Node... nodes) {
		return execute((Object[]) nodes) != null;
	}

	public boolean prove(QueryObject queryObject) {
		return execute(queryObject) != null;
	}

	/**
	 * Parses the nodes from the variable substitutions.
	 * 
	 * @param qo
	 *            The query object to run and parse.
	 * @param var
	 *            The variable substitutions to extract.
	 * @return The parsed results.
	 */
	public Collection<Node> executeAndParseVar(QueryObject qo, String var) {
		Collection<Substitution> subs = execute(qo);
		if (subs == null)
			return null;
		Collection<Node> results = new ArrayList<>(subs.size());
		for (Substitution s : subs) {
			Node res = s.getSubstitution(var);
			if (!results.contains(res))
				results.add(res);
		}
		return results;
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);

		initInferenceModules();
		for (QueryWorker qw : inferenceModules_.values())
			qw.setDAG(directedAcyclicGraph);
	}
}
