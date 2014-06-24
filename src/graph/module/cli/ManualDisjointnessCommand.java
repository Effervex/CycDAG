/*******************************************************************************
 * Copyright (C) 2013 University of Waikato, Hamilton, New Zealand.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *    Sam Sarjant - initial API and implementation
 ******************************************************************************/
package graph.module.cli;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collections;

import org.apache.commons.lang3.StringUtils;

import util.UtilityMethods;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;
import graph.core.cli.DAGPortHandler;
import graph.inference.QueryObject;
import graph.module.NLPToStringModule;
import core.Command;

public class ManualDisjointnessCommand extends Command {
	private static final File OUTPUT_FILE = new File("manualDisjointness.out");

	@Override
	public String helpText() {
		return super.helpText();
	}

	@Override
	public String shortDescription() {
		return "Provides a method for manually processing disjointness decisions.";
	}

	@Override
	protected void executeImpl() {
		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		// If file, load the file, iterate through it, and output to file.
		ArrayList<String> split = UtilityMethods.split(data, ' ');
		DirectedAcyclicGraph dag = ((DAGPortHandler) handler).getDAG();
		try {
			OUTPUT_FILE.createNewFile();
			BufferedWriter out = new BufferedWriter(new FileWriter(OUTPUT_FILE));
			if (split.size() == 1) {
				File file = new File(split.get(0));
				if (!file.exists()) {
					print("-1|Cannot find file.\n");
					out.close();
					return;
				}
				BufferedReader in = new BufferedReader(new FileReader(file));

				String input = null;
				ArrayList<String> inputs = new ArrayList<>();
				while ((input = in.readLine()) != null) {
					if (!input.startsWith("@")) {
						inputs.add(input);
					} else {
						out.write(input + "\n");
					}
				}
				in.close();

				// Shuffle and process
				Collections.shuffle(inputs);
				int i = 1;
				for (String line : inputs) {
					String[] elements = line.split(",");
					if (!printResult(elements[0], elements[1], dag, out, i++,
							inputs.size()))
						break;
				}
			} else if (split.size() == 2) {
				// If inputs, process, and output to file.
				printResult(split.get(0), split.get(1), dag, out, 1, 1);
			}
			out.close();
		} catch (Exception e) {
			print("-1|Exception!\n");
			return;
		}
	}

	private boolean printResult(String conceptA, String conceptB,
			DirectedAcyclicGraph dag, BufferedWriter out, int index, int total)
			throws Exception {
		int decision = processDisjointPair(conceptA, conceptB, dag, index,
				total);
		if (decision == -13)
			return false;

		out.write(conceptA + "," + conceptB + "," + decision + "\n");
		if (decision == 1) {
			print(conceptA + " is disjoint to " + conceptB + "\n");
		} else if (decision == 0) {
			print(conceptA + " is NOT disjoint to " + conceptB + "\n");
		} else if (decision == -1) {
			print("Cannot ascertain if " + conceptA + " is disjoint to "
					+ conceptB + "\n");
		}
		return true;
	}

	private int processDisjointPair(String conceptA, String conceptB,
			DirectedAcyclicGraph dag, int index, int total) throws Exception {
		NLPToStringModule nlpModule = (NLPToStringModule) dag
				.getModule(NLPToStringModule.class);
		DAGNode nodeA = (DAGNode) dag.findOrCreateNode(conceptA, null, false);
		DAGNode nodeB = (DAGNode) dag.findOrCreateNode(conceptB, null, false);

		BufferedReader in = getPortHandler().getReader();
		QueryObject qo = new QueryObject(
				CommonConcepts.DISJOINTWITH.getNode(dag), nodeA, nodeB);
		print("("
				+ index
				+ "/"
				+ total
				+ ") "
				+ StringUtils.capitalize(nlpModule.edgeToString(qo, true,
						false, false)) + " " + qo.toString()
				+ "\n(1:disjoint, 0:not disjoint, -1/EMPTY:unknown, Q:quit): ");

		flushOut();
		String decision = in.readLine().trim();
		if (decision.equalsIgnoreCase("Q"))
			return -13;
		else if (decision.equals("0"))
			return 0;
		else if (decision.equals("1"))
			return 1;
		else
			return -1;
	}
}
