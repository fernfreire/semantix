object IsolatedNodeDistance {
  val Infinity = 1e10
}

class Node(i: Int) {
  var linkedToNodes: List[Node] = List()

  val tag = this.i

  def addLink(n: Node): Unit = { if (!this.linkedToNodes.contains(n)) this.linkedToNodes ++= List(n) }

  def BFS(graph: Nodes): List[List[Node]] = {

    def BFS0(elems: List[Node], visited: List[List[Node]]): List[List[Node]] = {
      val newNeighbors = graph.listOfNodes.filter(elems.contains(_)).flatMap(_.linkedToNodes).filterNot(visited.flatten.contains(_)).groupBy(_.tag).map(_._2.head).toList
      if (newNeighbors.isEmpty)
        visited
      else
        BFS0(newNeighbors, newNeighbors :: visited)
    }

    BFS0(List(this), List(List(this))).reverse
  }

  def dotProduct(as: List[Int], bs: List[Int]) = {
    require(as.size == bs.size)
    (for ((a, b) <- as zip bs) yield a * b) sum
  }

  def farness(graph: Nodes): Int = {
    val bfs = this.BFS(graph).map(_.size)
    val countList = List.range(0, bfs.size)
    dotProduct(bfs, countList)
  }

  def centrality(graph: Nodes) = {
    val thisFarness = farness(graph).toDouble
    if (thisFarness == 0) IsolatedNodeDistance.Infinity else (1 / thisFarness)
  }

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Node] && (this.tag == that.asInstanceOf[Node].tag)
  }

  override def toString = s"Node(${this.tag})"
}