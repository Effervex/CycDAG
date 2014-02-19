package graph.core.cli.comparator;

import graph.core.cli.comparator.StringCaseInsComparator;

public class CycStringCaseInsComparator extends StringCaseInsComparator {
	@Override
	protected int compareInternal(Object o1, Object o2) {
		String o1Str = CycStringComparator.processString(o1);
		String o2Str = CycStringComparator.processString(o2);
		return super.compareInternal(o1Str, o2Str);
	}
}
