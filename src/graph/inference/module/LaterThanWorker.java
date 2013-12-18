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
package graph.inference.module;

import graph.core.DAGNode;
import graph.inference.QueryObject;
import graph.inference.QueryWorker;
import graph.inference.Substitution;
import graph.module.QueryModule;

import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class LaterThanWorker extends QueryWorker {
	private static final long serialVersionUID = 4777969671003305047L;
	private static final String[] DATE_PARSE_INTERVALS = { "'SecondFn' ss",
			"'MinuteFn' mm", "'HourFn' HH", "'DayFn' dd", "'MonthFn' MMMM",
			"'YearFn' yyyy" };

	public LaterThanWorker(QueryModule queryModule) {
		super(queryModule);
	}

	@Override
	public void queryInternal(QueryObject queryObj)
			throws IllegalArgumentException {
		DAGNode dateA = (DAGNode) queryObj.getNode(1);
		DAGNode dateB = (DAGNode) queryObj.getNode(2);

		DateTime now = DateTime.now();

		Interval dtA = parseDate(dateA, now);
		Interval dtB = parseDate(dateB, now);

		// If date A is later than date B, return true.
		if (dtA != null && dtB != null && dtA.isAfter(dtB))
			queryObj.addResult(new Substitution(), queryObj.getNodes());
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
			StringBuffer newPattern = new StringBuffer("("
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
}
