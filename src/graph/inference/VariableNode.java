package graph.inference;

import graph.core.StringNode;

public class VariableNode extends StringNode {
	public VariableNode(String string) {
		super(string);
	}

	private static final long serialVersionUID = -918899352582359058L;
	public static final VariableNode DEFAULT = new VariableNode("?X");

	@Override
	public String toString() {
		return getName();
	}

}
