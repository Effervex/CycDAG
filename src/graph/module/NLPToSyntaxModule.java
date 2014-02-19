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
package graph.module;

import graph.core.Edge;

import java.text.Normalizer;
import java.util.regex.Pattern;

import org.apache.commons.lang3.text.WordUtils;

public class NLPToSyntaxModule extends DAGModule<Edge> {
	private static final long serialVersionUID = 7531237954403448195L;

	@Override
	public Edge execute(Object... args) throws IllegalArgumentException,
			ModuleException {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * Replaces special characters from the latin-1 table with the nearest
	 * characters from the ascii table:
	 * 
	 * For example: �,�,�,� will become a, � becomes ss, �,�
	 * become c, C...
	 */
	public static String convertToAscii(String str) {
		if (str == null || str.isEmpty())
			return str;
		// Pre-normalisation
		String temp = Normalizer.normalize(str, Normalizer.Form.NFD);
		Pattern pattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");
		str = pattern.matcher(temp).replaceAll("");
	
		StringBuffer buffer = new StringBuffer();
		char[] strArray = str.toCharArray();
		for (int i = 0; i < strArray.length; i++) {
			char c = strArray[i];
	
			if (c < 128)
				buffer.append(c);
			else if (c == 176)
				buffer.append(" degrees ");
			else if (c == 198)
				buffer.append("AE");
			else if (c == 230)
				buffer.append("ae");
			else if (c == 338)
				buffer.append("OE");
			else if (c == 339)
				buffer.append("oe");
			else if (c == 223)
				buffer.append("ss");
			else if (c == 8211 || c == 8212)
				buffer.append("-");
			else if (c == 8217)
				buffer.append("'");
			else {
				// System.out.println("Unknown character: " + c + " (" + ((int)
				// c)
				// + ")" + ((c + "") == "?"));
				buffer.append("?");
			}
		}
		return buffer.toString();
	}

	public static String textToConcept(String text) {
		text = convertToAscii(text);
		if (text.replaceAll("!", "").equals("")) {
			text = text.replaceAll("!", "Exclm");
		} else {
			text = text.replaceAll("!", "");
		}
		text = text.replaceAll("&", " And ");
		text = text.replaceAll("@", " At ");
		text = text.replaceAll("\\+", " Plus ");
		text = text.replaceAll("'", "");
		text = text.replaceAll("\\.", "");
		// Remove other punctuation
		text = text.replaceAll("[^A-Za-z0-9 ()-]", " ");
		
		text = text.replaceAll("\\(", "- ");
		text = text.replaceAll("\\)", "");

		// Remove whitespace
		text = WordUtils.capitalize(text);
		text = text.replaceAll("\\s+", "");
		return text;
	}

}
