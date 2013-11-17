package graph.module.cli;

import graph.core.DAGNode;
import graph.core.cli.DAGPortHandler;
import graph.module.DAGModule;
import graph.module.DateParseModule;
import graph.module.ModuleException;
import graph.module.NodeAliasModule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import util.UtilityMethods;

public class FindCycNodeCommand extends FindNodeByAliasCommand {
	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		Collection<DAGModule<Collection<DAGNode>>> aliasModules = null;
		try {
			aliasModules = getAllAliasModules(dagHandler);
		} catch (ModuleException me) {
			print(me.getMessage() + "\n");
			return;
		}

		ArrayList<String> split = UtilityMethods.split(data, ' ');
		String alias = split.get(0);
		if (alias.matches(DAGNode.QUOTED_NAME.pattern()))
			alias = UtilityMethods.shrinkString(alias, 1);
		boolean caseSensitive = true;
		if (split.size() >= 2 && split.get(1).equals("F"))
			caseSensitive = false;
		boolean exactString = true;
		if (split.size() >= 3 && split.get(2).equals("F"))
			exactString = false;

		SortedSet<DAGNode> nodes = new TreeSet<>();
		for (DAGModule<Collection<DAGNode>> aliasModule : aliasModules)
			nodes.addAll(aliasModule.execute(alias, caseSensitive, exactString));

		print(nodes.size() + "|");
		for (DAGNode n : nodes)
			print(dagHandler.textIDObject(n) + "|");
		print("\n");
	}

	private Collection<DAGModule<Collection<DAGNode>>> getAllAliasModules(
			DAGPortHandler dagHandler) {
		Collection<DAGModule<Collection<DAGNode>>> aliasModules = new ArrayList<>();

		NodeAliasModule aliasModule = (NodeAliasModule) dagHandler.getDAG()
				.getModule(NodeAliasModule.class);
		if (aliasModule == null)
			throw new ModuleException(
					"Node Alias module is not in use for this DAG.");
		aliasModules.add(aliasModule);

		DateParseModule dateModule = (DateParseModule) dagHandler.getDAG()
				.getModule(DateParseModule.class);
		if (dateModule == null)
			throw new ModuleException(
					"Date Parse module is not in use for this DAG.");
		aliasModules.add(dateModule);

		return aliasModules;
	}
}
