package graph.module;

import graph.core.CommonConcepts;
import graph.core.DAGNode;

import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;

/**
 * Parses a date from a string in a date parsable form and represents it with
 * functions.
 * 
 * @author Sam Sarjant
 */
public class DateParseModule extends DAGModule<Collection<DAGNode>> {
	private static final long serialVersionUID = 6113514387314037121L;
	private transient Collection<SimpleDateFormat> acceptedFormats_;
	private static final SimpleDateFormat MONTH_FORMATTER = new SimpleDateFormat(
			"MMMM");

	public DateParseModule() {
		initFormats();
	}

	private synchronized void initFormats() {
		if (acceptedFormats_ != null)
			return;
		acceptedFormats_ = new ArrayList<>();
		acceptedFormats_.add(new SimpleDateFormat("yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("yyyy MM"));
		acceptedFormats_.add(new SimpleDateFormat("yyyy MM dd"));
		acceptedFormats_.add(new SimpleDateFormat("MM"));
		acceptedFormats_.add(new SimpleDateFormat("MMMM"));
		acceptedFormats_.add(new SimpleDateFormat("MM yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("MMMM yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("MMMM dd yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("EEEE MMMM dd yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("dd MM yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("MM dd yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("dd MMMM yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("EEEE dd MM yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("EEEE dd MMMM yyyy"));
		acceptedFormats_.add(new SimpleDateFormat("dd MMMM"));
		acceptedFormats_.add(new SimpleDateFormat("dd MM"));

		for (SimpleDateFormat sdf : acceptedFormats_) {
			sdf.setLenient(false);
		}
	}

	@Override
	public Collection<DAGNode> execute(Object... args)
			throws IllegalArgumentException, ModuleException {
		if (args == null || args.length == 0)
			throw new IllegalArgumentException(
					"Requires at least 1 string argument.");
		String dateStr = (String) args[0];

		return parseDate(dateStr);
	}

	public Collection<DAGNode> parseDate(String dateStr) {
		dateStr = dateStr.replaceAll("[ /\\-,]+", " ");
		// Split strings
		Collection<DAGNode> nodes = new HashSet<>();
		initFormats();
		for (SimpleDateFormat sdf : acceptedFormats_) {
			DAGNode date = parseDate(sdf, dateStr);
			if (date != null)
				nodes.add(date);
		}
		return nodes;
	}

	@SuppressWarnings("deprecation")
	private DAGNode parseDate(SimpleDateFormat sdf, String dateStr) {
		try {
			ParsePosition position = new ParsePosition(0);
			Date date = sdf.parse(dateStr, position);
			if (position.getIndex() != dateStr.length()) {
				// Throw an exception or whatever else you want to do
				return null;
			}
			String pattern = sdf.toPattern();

			StringBuffer buffer = new StringBuffer();
			boolean addFurther = false;
			int brackets = 0;
			if (addFurther || pattern.contains("d")) {
				addFurther = true;
				buffer.append("(" + CommonConcepts.DAYFN.getID() + " '"
						+ date.getDate() + " ");
				brackets++;
			}
			if (addFurther || pattern.contains("M")) {
				addFurther = true;
				buffer.append("(" + CommonConcepts.MONTHFN.getID() + " "
						+ MONTH_FORMATTER.format(date) + " ");
				brackets++;
			}
			if (pattern.contains("y")) {
				buffer.append("(" + CommonConcepts.YEARFN.getID() + " '"
						+ (date.getYear() + 1900));
				brackets++;
			} else if (addFurther)
				buffer.append(CommonConcepts.THEYEAR.getID());
			for (int i = 0; i < brackets; i++)
				buffer.append(")");
			return (DAGNode) dag_.findOrCreateNode(buffer.toString(), null,
					false, true, false);
		} catch (Exception e) {
		}
		return null;
	}

}
