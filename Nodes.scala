class Nodes {
  var listOfNodes: List[Node] = List()

  def addToList(a: Node): Unit = { if (!this.listOfNodes.contains(a)) this.listOfNodes ++= List(a) }

  def addPair(a: Node, b: Node): Unit = {
    if (a.tag > b.tag) this.addPair(b, a)
    else if (a.tag < b.tag) {
      this.addToList(a)
      this.addToList(b)
      this.listOfNodes.foreach(n => if (n == a) { n.addLink(b) })
      this.listOfNodes.foreach(n => if (n == b) { n.addLink(a) })
    } else return
  }

  def centralityList = this.listOfNodes.map(n => (n.centrality, n)).toList.sortBy(e => e._1)
  def printCentralityList = this.centralityList.foreach(e => println(s"${e._2} - centrality: ${(e._1 * 100 floor) / 100}"))
  def printLinks = this.listOfNodes.foreach(n => println(s"${n}, linked to ${n.linkedToNodes.mkString(", ")}"))

  override def toString = this.listOfNodes.mkString(", ")
}