package graph.core.cli.comparator;

import graph.core.cli.comparator.StringCaseInsComparator;

public class CycStringCaseInsComparator extends StringCaseInsComparator {
	@Override
	protected int compareInternal(Object o1, Object o2) {
		String o1Str = o1.toString();
		String o2Str = o2.toString();
		while (o1Str.startsWith("(") && o1Str.length() > 2)
			o1Str = o1Str.substring(1);
		while (o2Str.startsWith("(") && o2Str.length() > 2)
			o2Str = o2Str.substring(1);
		return super.compareInternal(o1Str, o2Str);
	}
}
