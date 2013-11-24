package graph.core;

import graph.inference.QueryObject;
import graph.module.QueryModule;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;

public class OntologyFunction extends DAGNode implements Edge {
	private static final long serialVersionUID = 473544398260462641L;
	protected Node[] nodes_;
	private Boolean anonymous_;

	public OntologyFunction() {
		super();
	}

	public OntologyFunction(Node... nodes) {
		super();
		nodes_ = nodes;
	}

	@Override
	public int compareTo(DAGObject o) {
		int result = super.compareTo(o);
		if (result != 0)
			return result;

		return toString().compareTo(o.toString());
	}

	@Override
	public boolean containsNode(Node node) {
		for (Node edgeNode : getNodes())
			if (edgeNode.equals(node))
				return true;
		return false;
	}

	@Override
	public Node[] getNodes() {
		return nodes_;
	}

	@Override
	public String getIdentifier() {
		if (!isAnonymous())
			return id_ + "";

		StringBuffer buffer = new StringBuffer("(");
		boolean first = true;
		for (Node n : nodes_) {
			if (!first)
				buffer.append(" ");
			buffer.append(n.getIdentifier());
			first = false;
		}
		buffer.append(")");
		return buffer.toString();
	}

	public boolean isAnonymous(DirectedAcyclicGraph dag) {
		// Check if function is unreifiable
		if (anonymous_ == null) {
			QueryModule queryModule = (QueryModule) dag
					.getModule(QueryModule.class);
			QueryObject queryObj = new QueryObject(
					CommonConcepts.ISA.getNode(dag), nodes_[0],
					CommonConcepts.UNREIFIABLE_FUNCTION.getNode(dag));
			queryModule.applyModule(
					CommonConcepts.ASSERTED_SENTENCE.getNodeName(), queryObj);
			anonymous_ = queryObj.getResults() != null;
			if (anonymous_)
				id_ = -1;
		}
		return anonymous_;
	}

	@Override
	public String getName() {
		return "(" + StringUtils.join(nodes_, ' ') + ")";
	}

	@Override
	public String toString(boolean useIDs) {
		if (!useIDs)
			return toString();
		StringBuffer buffer = new StringBuffer("(");
		boolean first = true;
		for (Node n : nodes_) {
			if (!first)
				buffer.append(" ");
			buffer.append(n.getIdentifier());
			first = false;
		}
		buffer.append(")");
		return buffer.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = prime + Arrays.hashCode(nodes_);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		OntologyFunction other = (OntologyFunction) obj;
		if (!Arrays.equals(nodes_, other.nodes_))
			return false;
		return true;
	}

	@Override
	protected void writeFullObject(ObjectOutput out) throws IOException {
		super.writeFullObject(out);
		out.writeObject(nodes_);
		out.writeBoolean(anonymous_);
	}

	@Override
	protected void readFullObject(ObjectInput in) throws IOException,
			ClassNotFoundException {
		super.readFullObject(in);
		nodes_ = (Node[]) in.readObject();
		anonymous_ = in.readBoolean();
	}
}
