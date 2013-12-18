/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand
 ******************************************************************************/
package graph.module.cli;

import graph.core.DAGNode;
import graph.core.cli.DAGPortHandler;
import graph.module.DAGModule;
import graph.module.DateParseModule;
import graph.module.ModuleException;

import java.util.Collection;

public class FindCycNodeCommand extends FindNodeByAliasCommand {
	@Override
	protected Collection<DAGModule<Collection<DAGNode>>> getAllAliasModules(
			DAGPortHandler dagHandler) {
		Collection<DAGModule<Collection<DAGNode>>> aliasModules = super
				.getAllAliasModules(dagHandler);

		DateParseModule dateModule = (DateParseModule) dagHandler.getDAG()
				.getModule(DateParseModule.class);
		if (dateModule == null)
			throw new ModuleException(
					"Date Parse module is not in use for this DAG.");
		aliasModules.add(dateModule);

		return aliasModules;
	}
}
