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
package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import graph.core.CommonConcepts;
import graph.core.CycDAG;
import graph.core.DAGNode;
import graph.core.Node;
import graph.core.StringNode;
import graph.module.DateParseModule;

import java.io.File;
import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class DateParseModuleTest {
	private DateParseModule sut_;
	private CycDAG dag_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		CommonConcepts.initialise(dag_);
		sut_ = (DateParseModule) dag_.getModule(DateParseModule.class);
		sut_.clear();
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
		sut_.clear();
	}

	@Test
	public void test() {
		Node creator = new StringNode("Creator");
		dag_.findOrCreateNode("February", creator, true, true, false);
		dag_.findOrCreateNode("January", creator, true, true, false);

		// Year
		Collection<DAGNode> nodes = sut_.parseDate("1987");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode("(YearFn '1987)",
				creator, false, true, false)));
		nodes = sut_.parseDate("2013");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode("(YearFn '2013)",
				creator, false, true, false)));
		nodes = sut_.parseDate("2");
		assertEquals(nodes.size(), 2);
		assertTrue(nodes.contains(dag_.findOrCreateNode("(YearFn '2)", creator,
				false, true, false)));
		assertTrue(nodes.contains(dag_.findOrCreateNode("(MonthFn February "
				+ CommonConcepts.THE_YEAR.getID() + ")", creator, false, true,
				false)));
		assertTrue(nodes.contains(dag_.findOrCreateNode("(YearFn '2)", creator,
				false, true, false)));

		// Months
		nodes = sut_.parseDate("February");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode("(MonthFn February "
				+ CommonConcepts.THE_YEAR.getID() + ")", creator, false, true,
				false)));
		nodes = sut_.parseDate("02");
		assertEquals(nodes.size(), 2);
		assertTrue(nodes.contains(dag_.findOrCreateNode("(MonthFn February "
				+ CommonConcepts.THE_YEAR.getID() + ")", creator, false, true,
				false)));
		assertTrue(nodes.contains(dag_.findOrCreateNode("(YearFn '2)", creator,
				false, true, false)));
		nodes = sut_.parseDate("Feb");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode("(MonthFn February "
				+ CommonConcepts.THE_YEAR.getID() + ")", creator, false, true,
				false)));

		// Month-Year
		nodes = sut_.parseDate("Feb 2013");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(MonthFn February (YearFn '2013))", creator, false, true,
				false)));
		nodes = sut_.parseDate("2-2013");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(MonthFn February (YearFn '2013))", creator, false, true,
				false)));
		nodes = sut_.parseDate("2/2013");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(MonthFn February (YearFn '2013))", creator, false, true,
				false)));

		nodes = sut_.parseDate("1 February, 1479");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(DayFn '1 (MonthFn February (YearFn '1479)))", creator, false,
				true, false)));
		nodes = sut_.parseDate("Monday, 1 February, 1479");
		assertEquals(nodes.size(), 1);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(DayFn '1 (MonthFn February (YearFn '1479)))", creator, false,
				true, false)));
		nodes = sut_.parseDate("1-2-1479");
		assertEquals(nodes.size(), 2);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(DayFn '1 (MonthFn February (YearFn '1479)))", creator, false,
				true, false)));
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(DayFn '2 (MonthFn January (YearFn '1479)))", creator, false,
				true, false)));
		nodes = sut_.parseDate("01-02-1479");
		assertEquals(nodes.size(), 2);
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(DayFn '1 (MonthFn February (YearFn '1479)))", creator, false,
				true, false)));
		assertTrue(nodes.contains(dag_.findOrCreateNode(
				"(DayFn '2 (MonthFn January (YearFn '1479)))", creator, false,
				true, false)));
	}

}
