package graph.module;

public class UnknownMarkupException extends Exception {
	private static final long serialVersionUID = 1L;
	
	public UnknownMarkupException() {
		super("Could not parse special markup.");
	}
}
