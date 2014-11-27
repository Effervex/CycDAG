package graph.inference.module;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.math.NumberUtils;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import graph.core.CommonConcepts;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.PrimitiveNode;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

/**
 * A class for performing queries involving evaluatable predicates that return
 * true or false. Including different, equals, lessThan, stringSubword, etc.
 * 
 * @author Sam Sarjant
 */
public class EvaluatablePredicateWorker extends QueryWorker {
	private static final String[] DATE_PARSE_INTERVALS = { "'SecondFn' ss",
			"'MinuteFn' mm", "'HourFn' HH", "'DayFn' dd", "'MonthFn' MMMM",
			"'YearFn' yyyy" };

	private static final long serialVersionUID = 1L;

	public EvaluatablePredicateWorker(QueryModule queryModule) {
		super(queryModule);
	}

	private Interval parseDate(DAGNode date, DateTime now) {
		String dateStr = date.toString();
		if (dateStr.equals("Now") || dateStr.equals("Now-Generally"))
			return new Interval(now.getMillis(), now.getMillis() + 1);
		if (dateStr.equals("Today-Indexical"))
			return new Interval(now.dayOfYear().roundFloorCopy(), now
					.dayOfYear().roundCeilingCopy());
		if (dateStr.equals("Tomorrow-Indexical")) {
			return new Interval(now.plusDays(1).dayOfYear().roundFloorCopy(),
					now.plusDays(1).dayOfYear().roundCeilingCopy());
		}
		if (dateStr.equals("Yesterday-Indexical")) {
			return new Interval(now.minusDays(1).dayOfYear().roundFloorCopy(),
					now.minusDays(1).dayOfYear().roundCeilingCopy());
		}
		if (dateStr.equals("TheYear-Indexical")) {
			return new Interval(now.year().roundFloorCopy(), now.year()
					.roundCeilingCopy());
		}

		// Parse the date from the DAGNode
		String parsePattern = null;
		for (int i = DATE_PARSE_INTERVALS.length - 1; i >= 0; i--) {
			StringBuilder newPattern = new StringBuilder("("
					+ DATE_PARSE_INTERVALS[i]);
			if (parsePattern != null)
				newPattern.append(" " + parsePattern);
			newPattern.append(")");
			parsePattern = newPattern.toString();

			DateTimeFormatter dtf = DateTimeFormat.forPattern(parsePattern);
			try {
				DateTime dateTime = dtf.parseDateTime(dateStr);
				if (dateTime != null) {
					switch (i) {
					case 0:
						return new Interval(dateTime.getMillis(), dateTime
								.plusSeconds(1).minusMillis(1).getMillis());
					case 1:
						return new Interval(dateTime.getMillis(), dateTime
								.plusMinutes(1).minusMillis(1).getMillis());
					case 2:
						return new Interval(dateTime.getMillis(), dateTime
								.plusHours(1).minusMillis(1).getMillis());
					case 3:
						return new Interval(dateTime.getMillis(), dateTime
								.plusDays(1).minusMillis(1).getMillis());
					case 4:
						return new Interval(dateTime.getMillis(), dateTime
								.plusMonths(1).minusMillis(1).getMillis());
					case 5:
						return new Interval(dateTime.getMillis(), dateTime
								.plusYears(1).minusMillis(1).getMillis());
					}
				}
			} catch (Exception e) {
			}
		}
		return null;
	}

	protected void differentWorker(QueryObject queryObj) {
		Node[] nodes = queryObj.getNodes();
		Set<Node> nodeEquals = new HashSet<>();
		for (int i = 1; i < nodes.length; i++) {
			if (!nodeEquals.add(nodes[i]))
				return;
		}
		queryObj.addResult(new Substitution(), nodes);
	}

	protected void equalsWorker(QueryObject queryObj) {
		Node[] nodes = queryObj.getNodes();
		for (int i = 2; i < nodes.length; i++) {
			if (!nodes[i].equals(nodes[i - 1]))
				return;
		}
		queryObj.addResult(new Substitution(), nodes);
	}

	protected void laterThanWorker(QueryObject queryObj) {
		if (!(queryObj.getNode(1) instanceof DAGNode && queryObj.getNode(2) instanceof DAGNode))
			return;
		DAGNode dateA = (DAGNode) queryObj.getNode(1);
		DAGNode dateB = (DAGNode) queryObj.getNode(2);

		DateTime now = DateTime.now();

		Interval dtA = parseDate(dateA, now);
		Interval dtB = parseDate(dateB, now);

		// If date A is later than date B, return true.
		if (dtA != null && dtB != null && dtA.isAfter(dtB))
			queryObj.addResult(new Substitution(), queryObj.getNodes());
	}

	protected void numericalWorker(CommonConcepts cc, QueryObject queryObj) {
		if (queryObj.getNodes().length != 3)
			return;
		Node n1 = queryObj.getNode(1);
		Node n2 = queryObj.getNode(2);
		if (!(n1 instanceof PrimitiveNode) || !(n2 instanceof PrimitiveNode))
			return;
		PrimitiveNode pn1 = (PrimitiveNode) n1;
		PrimitiveNode pn2 = (PrimitiveNode) n2;
		// Is it a number?
		if (!Number.class.isAssignableFrom(pn1.getPrimitive().getClass())
				|| !Number.class
						.isAssignableFrom(pn2.getPrimitive().getClass()))
			return;
		double d1 = ((Number) pn1.getPrimitive()).doubleValue();
		double d2 = ((Number) pn2.getPrimitive()).doubleValue();

		switch (cc) {
		case LESS_THAN:
			if (d1 < d2)
				queryObj.addResult(new Substitution(), queryObj.getNodes());
			break;
		case LESS_THAN_EQUAL:
			if (d1 <= d2)
				queryObj.addResult(new Substitution(), queryObj.getNodes());
			break;
		case GREATER_THAN:
			if (d1 > d2)
				queryObj.addResult(new Substitution(), queryObj.getNodes());
			break;
		case GREATER_THAN_EQUAL:
			if (d1 >= d2)
				queryObj.addResult(new Substitution(), queryObj.getNodes());
			break;
		default:
			break;
		}
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		Node evalPred = queryObj.getNode(0);
		if (evalPred.equals(CommonConcepts.DIFFERENT.getNode(dag_)))
			differentWorker(queryObj);
		else if (evalPred.equals(CommonConcepts.EQUALS.getNode(dag_))
				|| evalPred.equals(CommonConcepts.NUMERICALLY_EQUAL
						.getNode(dag_)))
			equalsWorker(queryObj);
		else if (evalPred.equals(CommonConcepts.LATER_PREDICATE.getNode(dag_)))
			laterThanWorker(queryObj);
		else if (evalPred.equals(CommonConcepts.LESS_THAN.getNode(dag_)))
			numericalWorker(CommonConcepts.LESS_THAN, queryObj);
		else if (evalPred.equals(CommonConcepts.GREATER_THAN.getNode(dag_)))
			numericalWorker(CommonConcepts.GREATER_THAN, queryObj);
		else if (evalPred.equals(CommonConcepts.LESS_THAN_EQUAL.getNode(dag_)))
			numericalWorker(CommonConcepts.LESS_THAN_EQUAL, queryObj);
		else if (evalPred.equals(CommonConcepts.GREATER_THAN_EQUAL
				.getNode(dag_)))
			numericalWorker(CommonConcepts.GREATER_THAN_EQUAL, queryObj);
		else
			querier_.applyModule(QueryModule.DEFAULT_WORKER, queryObj);
	}
}
