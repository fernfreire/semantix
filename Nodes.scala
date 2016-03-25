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

  def centralityList = this.listOfNodes.map(n => (n.centrality(this), n)).toList.sortBy(e => e._1).reverse
  def printCentralityList = this.centralityList.foreach(e => println(s"${e._2} - centrality: ${(e._1 * 10000 floor) / 10000}"))
  def closestToCenterList = {
    val cl = this.centralityList
    cl.filter(_._1.equals(cl.head._1))
  }
  def printClosestToCenterList = this.closestToCenterList.foreach(e => println(s"${e._2} - centrality: ${(e._1 * 10000 floor) / 10000}"))
  def printLinks = this.listOfNodes.foreach(n => println(s"${n}, linked to ${n.linkedToNodes.mkString(", ")}"))

  override def toString = this.listOfNodes.mkString(", ")
}