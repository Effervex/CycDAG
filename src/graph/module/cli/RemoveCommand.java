package graph.module.cli;

import java.io.BufferedReader;
import java.util.Collection;
import java.util.HashSet;

import core.Command;
import core.CommandParser;

public class RemoveCommand extends Command {
	@Override
	public String helpText() {
		return "{1} <commandA + args>\n<commandB + args> : "
				+ "Removes the output of commandB from commandA's output.";
	}

	@Override
	public String shortDescription() {
		return "Removes the output of one command from the output of another command.";
	}

	@Override
	protected void executeImpl() {
		// Format each element into individual commands
		if (data.length() == 0) {
			print("-1|No command or delimiter specified.\n");
			return;
		}

		try {
			Command commandA = CommandParser.parse(data);
			commandA.setPortHandler(handler);
			commandA.execute();
			String resultA = commandA.getResult();
			String[] resultASplit = resultA.split("\\|");
			Collection<String> resultACol = new HashSet<>();
			for (int i = 1; i < resultASplit.length; i++) {
				resultACol.add(resultASplit[i]);
			}

			BufferedReader in = getPortHandler().getReader();
			String commandBStr = in.readLine().trim();
			Command commandB = CommandParser.parse(commandBStr);
			commandB.setPortHandler(handler);
			commandB.execute();
			String resultB = commandB.getResult();
			String[] resultBSplit = resultB.split("\\|");
			Collection<String> resultBCol = new HashSet<>();
			for (int i = 1; i < resultBSplit.length; i++) {
				String single = resultBSplit[i].trim();
				if (!single.isEmpty())
					resultACol.add(single);
			}

			resultACol.removeAll(resultBCol);
			print(resultACol.size() + "|");
			for (String str : resultACol)
				print(str + "|");
			print("\n");
		} catch (Exception e) {
			print("-1|Error while parsing arguments.\n");
		}
	}

}
