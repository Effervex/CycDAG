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
import static org.junit.Assert.assertNotNull;
import graph.core.CycDAG;
import graph.core.DirectedAcyclicGraph;
import graph.module.NLPToSyntaxModule;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class NLPToSyntaxModuleTest {
	private DirectedAcyclicGraph dag_;
	private NLPToSyntaxModule sut_;

	@Before
	public void setUp() throws Exception {
		dag_ = new CycDAG(new File("test"));
		sut_ = (NLPToSyntaxModule) dag_.getModule(NLPToSyntaxModule.class);
		assertNotNull(sut_);
	}

	@After
	public void tearDown() throws Exception {
		dag_.clear();
	}
	
	/**
	 * Tests the makeStringBasic method
	 */
	@Test
	public void testMakeStringBasic() {
		// Basic case
		assertEquals(NLPToSyntaxModule.textToConcept("Dog"), "Dog");
		// Removal of punctuation
		assertEquals(NLPToSyntaxModule.textToConcept("A.B.C."), "ABC");
		// And
		assertEquals(NLPToSyntaxModule.textToConcept("Cain & Abel"),
				"CainAndAbel");
		// Foreign characters
		assertEquals(NLPToSyntaxModule.textToConcept("Schrodinger's cat"),
				"SchrodingersCat");
		assertEquals(NLPToSyntaxModule.textToConcept("Andre Benoit"),
				"AndreBenoit");
	}

}
