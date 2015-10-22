package graph.module;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.ObjectInputStream;

import weka.classifiers.Classifier;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;
import graph.core.DAGEdge;
import graph.core.DAGNode;
import graph.core.DirectedAcyclicGraph;

/**
 * A class which uses Machine Learning to predict whether or not two collections
 * are disjoint.
 *
 * @author Sam Sarjant
 */
public class DisjointPredictorModule extends DAGModule<Double> {
	private static final long serialVersionUID = 1L;

	public static final File ARFF_LOCATION = new File("lib/disjointARFF.arff");
	public static final File CLASSIFIER_FILE = new File("lib/disjoints.model");

	/** Instance information. */
	private transient Instances arffData_;

	/** The core classifier. */
	private transient Classifier classifier_;

	/** The module containing the relation data. */
	private transient ConceptNetAnalyserV2Module assertionModule_;

	public DisjointPredictorModule() {
		try {
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(
					CLASSIFIER_FILE));

			arffData_ = new Instances(new FileReader(ARFF_LOCATION));
			arffData_.setClassIndex(arffData_.numAttributes() - 1);
			classifier_ = (Classifier) ois.readObject();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public Double execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		if (!(args.length == 2 && args[0] instanceof DAGNode && args[1] instanceof DAGNode))
			throw new IllegalArgumentException(
					"Requires two DAGNode arguments! " + args.toString());
		DAGNode nodeA = (DAGNode) args[0];
		DAGNode nodeB = (DAGNode) args[1];
		return classify(nodeA, nodeB);
	}

	/**
	 * Classifies whether two nodes are disjoint. Note disjointness can only be
	 * performed on Collections.
	 *
	 * @param conceptA
	 *            The first node.
	 * @param conceptB
	 *            The second node.
	 * @return The confidence of disjointness. A value of 1 is full confidence,
	 *         a value of 0 is no confidence. A value of -1 indicates
	 *         non-collections used.
	 */
	public synchronized double classify(DAGNode conceptA, DAGNode conceptB) {
		Instance[] instances = parseInstance(conceptA, conceptB);
		if (instances.length == 0)
			return -1;
		try {
			double minInst = 0.0;
			for (Instance inst : instances) {
				double classify = classifier_.classifyInstance(inst);
				minInst = Math.max(classify, minInst);
				if (minInst == 1)
					return 0;
			}
			return 1 - minInst;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return 0;
	}

	/**
	 * Parses the instance(s) from the two concepts. It could be multiple
	 * instances because there could be multiple common parents.
	 *
	 * @param conceptA
	 *            The first instance.
	 * @param conceptB
	 *            The second instance.
	 * @return An array of instances representing the concepts.
	 */
	public Instance[] parseInstance(DAGNode conceptA, DAGNode conceptB) {
		String[][] data = assertionModule_.getARFFData("IsA", conceptA,
				conceptB, null, true);
		Instance[] instances = new Instance[data.length];
		for (int i = 0; i < data.length; i++) {
			instances[i] = new DenseInstance(ARFFData.values().length);
			instances[i].setDataset(arffData_);
			for (int j = 0; j < data[i].length; j++) {
				if (!data[i][j].isEmpty()) {
					if (arffData_.attribute(i).isNumeric())
						instances[i]
								.setValue(i, Double.parseDouble(data[i][j]));
					else if (arffData_.attribute(i).isNominal()) {
						if (data[i][j].equals("?"))
							instances[i].setMissing(i);
						else {
							int relationIndex = arffData_.attribute(i)
									.indexOfValue(data[i][j]);
							instances[i].setValue(i, relationIndex);
						}
					} else {
						instances[i].setValue(i, data[i][j]);
					}
				}
			}
		}
		return instances;
	}

	@Override
	public boolean supportsEdge(DAGEdge edge) {
		return false;
	}

	@Override
	public boolean supportsNode(DAGNode node) {
		return false;
	}

	@Override
	public void setDAG(DirectedAcyclicGraph directedAcyclicGraph) {
		super.setDAG(directedAcyclicGraph);
		assertionModule_ = (ConceptNetAnalyserV2Module) directedAcyclicGraph
				.getModule(ConceptNetAnalyserV2Module.class);
	}
}
