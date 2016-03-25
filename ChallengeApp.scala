object ChallengeApp extends App {
  println(":q to quit this application\n:c to get centrality measure for nodes\n:c to print the central node and its centrality measure\n:cs to get the list of nodes ordered by the centrality measure\n:n to get a list of nodes on the graph\n:l to get a list of nodes with their links\ntype a pair of integers (separated by whitespace) to add the edge on the graph")
  val nodes = new Nodes
  var input = "blargh!"
  while (input != ":q") {
    input = scala.io.StdIn.readLine("input> ")
    if (input == ":cs") {
      nodes.printCentralityList
    } else if (input == ":c") {
      nodes.printClosestToCenterList
    } else if (input == ":n") {
      println(nodes)
    } else if (input == ":l") {
      nodes.printLinks
    } else if (input != ":q") {
      try {
        val vs = input.split(" ").map(i => i.toInt).toList
        nodes.addPair(new Node(vs(0)), new Node(vs(1)))
      } catch {
        case format: java.lang.NumberFormatException => println("Please type something valid!")
        case index: java.lang.IndexOutOfBoundsException => println("A single integer is not valid!")
      }
    } 
  }
}