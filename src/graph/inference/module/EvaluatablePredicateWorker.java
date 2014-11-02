package graph.inference.module;

import graph.core.CommonConcepts;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.module.QueryModule;

/**
 * A class for performing queries involving evaluatable predicates that return
 * true or false. Including different, equals, lessThan, stringSubword, etc.
 * 
 * @author Sam Sarjant
 */
public class EvaluatablePredicateWorker extends QueryWorker {
	private static final long serialVersionUID = 1L;

	public EvaluatablePredicateWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		if (queryObj.getNode(0).equals(CommonConcepts.DIFFERENT.getNode(dag_)))
			differentWorker(queryObj);
		else if (queryObj.getNode(0).equals(CommonConcepts.EQUALS.getNode(dag_)))
			equalsWorker(queryObj);
		else if (queryObj.getNode(0).equals(CommonConcepts.LATER_PREDICATE.getNode(dag_)))
			laterThanWorker(queryObj);
		// TODO Less than, greater than, >=, <=
	}

	private void laterThanWorker(QueryObject queryObj) {
		// TODO Auto-generated method stub
		
	}

	private void equalsWorker(QueryObject queryObj) {
		// TODO Auto-generated method stub
		
	}

	private void differentWorker(QueryObject queryObj) {
		// TODO Auto-generated method stub
		
	}

}
