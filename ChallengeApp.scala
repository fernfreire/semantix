object ChallengeApp extends App {
  println(":q to quit this application\n:c to get centrality measure for nodes\n:n to get a list of nodes on the graph\n:l to get a list of nodes with their links\ntype a pair of integers (separated by whitespace) to add the edge on the graph")
  val nodes = new Nodes
  var input = "blargh!"
  while (input != ":q") {
    input = scala.io.StdIn.readLine("input> ")
    if (input == ":c") {
      nodes.printCentralityList
    } else if (input == ":n") {
      println(nodes)
    } else if (input == ":l") {
      nodes.printLinks
    } else if (input != ":q") {
      try {
        val vs = input.split(" ").map(i => i.toInt).toList
        nodes.addPair(new Node(vs(0)), new Node(vs(1)))
      } catch {
        case ioe: java.lang.NumberFormatException => println("Please type something valid!")
      }
    } else {
    }
  }
}