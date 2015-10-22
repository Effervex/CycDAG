package graph.module.cli;

import graph.core.DAGNode;
import graph.core.cli.DAGCommand;
import graph.core.cli.DAGPortHandler;
import graph.module.DisjointPredictorModule;

import java.util.ArrayList;

import util.UtilityMethods;

/**
 * Applies the disjointness classifier to two collections to probabilistically
 * predict whether they are disjoint (instead of ontologically disjoint). This
 * has the advantage of inferring non-existent disjoint edges though this is
 * countered by the probabilistic nature of its predictions.
 *
 * @author Sam Sarjant
 */
public class PredictDisjointCommand extends DAGCommand {
	@Override
	public String helpText() {
		return "{} collectionA collectionB (threshold)? : Predicts whether "
				+ "collectionA and collectionB are disjoint, with a numerical "
				+ "confidence. If that confidence is greater than the "
				+ "threshold (default value 0.5), the concepts are considered "
				+ "disjoint.";
	}

	@Override
	public String shortDescription() {
		return "Predicts whether two collections are disjoint.";
	}

	@Override
	protected void executeImpl() {
		DAGPortHandler dagHandler = (DAGPortHandler) handler;
		DisjointPredictorModule predictorModule = (DisjointPredictorModule) dagHandler
				.getDAG().getModule(DisjointPredictorModule.class);
		if (predictorModule == null) {
			print("Disjoint Predictor Module is not in use for this DAG.\n");
			return;
		}

		if (data.isEmpty()) {
			printErrorNoData();
			return;
		}

		// Three args
		ArrayList<String> split = UtilityMethods.split(data, ' ');
		if (split.size() < 2 || split.size() > 3) {
			print("-1|Requires two Collection arguments (and maybe threshold).\n");
			return;
		}

		// Parse the concepts
		DAGNode conceptA = (DAGNode) dagHandler.getDAG().findOrCreateNode(
				split.get(0), null, false, false, true);
		if (conceptA == null) {
			print("-1|Could not parse " + split.get(0) + " as Collection.\n");
			return;
		}
		DAGNode conceptB = (DAGNode) dagHandler.getDAG().findOrCreateNode(
				split.get(1), null, false, false, true);
		if (conceptB == null) {
			print("-1|Could not parse " + split.get(1) + " as Collection.\n");
			return;
		}
		double threshold = 0.5;
		if (split.size() == 3)
			threshold = Double.parseDouble(split.get(2));

		// Make prediction
		double prediction = predictorModule.classify(conceptA, conceptB);
		if (prediction == -1) {
			print("-1|Error predicting disjointness between "
					+ dagHandler.textIDObject(conceptA) + " and "
					+ dagHandler.textIDObject(conceptB) + ".\n");
			return;
		}
		if (prediction >= threshold)
			print("1|T|");
		else
			print("0|NIL|");
		print(prediction + "\n");
	}
}
