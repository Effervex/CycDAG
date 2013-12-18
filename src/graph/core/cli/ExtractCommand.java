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
package graph.core.cli;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import core.Command;
import core.CommandParser;

public class ExtractCommand extends Command {
	public static Pattern ARG_PATTERN = Pattern.compile("(\\?\\S+)\\s(.+)");

	@Override
	public String helpText() {
		return "{0} var command : Converts a set of results in variable "
				+ "format (e.g. ?X/1234) into non-variable format.";
	}

	@Override
	public String shortDescription() {
		return "Extracts the values of variable substitutions from a command's output.";
	}

	@Override
	protected void executeImpl() {
		// Format each element into individual commands
		if (data.length() == 0) {
			print("No command or delimiter specified.\n");
			return;
		}
		Matcher m = ARG_PATTERN.matcher(data);
		if (!m.matches()) {
			print("No command or delimiter specified.\n");
			return;
		}

		String variable = m.group(1);
		String commandStr = m.group(2);

		Command command = CommandParser.parse(commandStr);
		command.setPortHandler(handler);
		command.execute();
		print(command.getResult().replaceAll(Pattern.quote(variable) + "/", ""));
	}

}
