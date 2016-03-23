object IsolatedNodeDistance {
  val Infinity = 1e10
}

class Node(i: Int) {
  var linkedToNodes: List[Node] = List()

  val tag = this.i

  def addLink(n: Node): Unit = { if (!this.linkedToNodes.contains(n)) this.linkedToNodes ++= List(n) }

  def farness = this.linkedToNodes.size.toDouble
  def centrality = if (farness == 0) IsolatedNodeDistance.Infinity else (1 / this.farness)

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Node] && (this.tag == that.asInstanceOf[Node].tag)
  }

  override def toString = s"Node(${this.tag})"
}