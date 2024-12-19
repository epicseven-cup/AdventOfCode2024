package Day5_PrintQueue

import scala.collection.mutable

class PrintQueue extends Solution {
  override def ProblemOne(filename: String): Unit = {
    val content: String = ReadRawPuzzel(filename)
    // Splitting on the empty new line, since the previous is a newline and the line after it was an empty new line this will work well
    val splitContent: Array[String] = content.split("\n\n")
    //    println(splitContent(0))
    //    println(splitContent(1))
    val ruleRaw: Array[String] = splitContent(0).strip().split("\n")
    val graph: mutable.Map[String, mutable.Set[String]] = mutable.Map[String, mutable.Set[String]]()
    val printsSet: mutable.Set[String] = mutable.Set()
    for (line <- ruleRaw) {
      val splitContent: Array[String] = line.split(raw"\|")
      //      println(splitContent.mkString("Array(", ", ", ")"))
      val preq: String = splitContent(0)
      val current: String = splitContent(1)
      val value: mutable.Set[String] = graph.getOrElse(preq, mutable.Set())
      value.add(current)
      graph.put(preq, value)
      printsSet.add(preq)
      printsSet.add(current)
    }

    for ((key, value) <- graph) {
      value.add(Integer.MAX_VALUE.toString)
    }

    graph.put(Integer.MAX_VALUE.toString, mutable.Set())

    var output: Int = 0
    // Going through the possible print order
    val answers: Array[String] = splitContent(1).split("\n")
    for (lines <- answers) {
      var ordering: Array[String] = lines.split(",")
      var queue: mutable.Queue[String] = mutable.Queue()
      val visited: mutable.Set[String] = mutable.Set()
      val root: String = ordering(0)
      // enqueuing node
      queue.enqueue(root)

      var expected: Int = 1
      ordering = ordering :+ Integer.MAX_VALUE.toString

      println(s"Expected: ${ordering.mkString("Array(", ", ", ")")}")
      while (queue.nonEmpty) {
        val current: String = queue.dequeue()
        //        visited.add(current)
        //        println(expected)
        if (expected == ordering.length) {
          queue = mutable.Queue()
        }
        //        println(s"Current: $current")
        val nodes: mutable.Set[String] = graph.getOrElse(current, mutable.Set())
        if (!visited.contains(ordering(expected)) && nodes.contains(ordering(expected)) && expected < ordering.length - 1) {
          //          println(nodes.toArray.mkString("Array(", ", ", ")"))
          queue.enqueue(ordering(expected))
          expected = expected + 1
        }

      }
      println(s"Final: $expected")
      println(s"Suppose to be: ${ordering.length - 1}")
      if (expected == ordering.length - 1) {
        println(ordering((ordering.length / 2) - 1))
        output += ordering((ordering.length / 2) - 1).toInt
      }


    }
    println(output)
    //    println(graph)
    //    println(printsSet)
  }

  private def _createPath(rootNode: String, nodes: Array[String], graph: mutable.Map[String, mutable.Set[String]]): List[String] = {
    var queue: mutable.Queue[(String, List[String], mutable.Set[String])] = mutable.Queue()
    val visited: mutable.Set[String] = mutable.Set()
    val root: String = rootNode
    // enqueuing node
    queue.enqueue((root, List(root), visited))
    var path: List[String] = List(root)
    while (queue.nonEmpty && visited.size < nodes.length) {
      val (current: String, cpath:List[String], cvisted:mutable.Set[String]) = queue.dequeue()
      val nextDoor: mutable.Set[String] = graph.getOrElse(current, mutable.Set())
//      println(s"NextDoor: ${nextDoor}")
      for (n <- nextDoor) {
//        println(s"Path: $path")
        if (!cvisted.contains(n) && nodes.contains(n)) {
          val currentVisted:mutable.Set[String] = cvisted.clone()
          currentVisted.add(n)
//          println(s"Current Path: $cpath")
          queue.enqueue((n, cpath :+ n, currentVisted))
          if ((cpath :+ n).length > path.length){
            path = cpath :+ n
          }
        }
      }
    }
    path
  }

  override def ProblemTwo(filename: String): Unit = {
    val content: String = ReadRawPuzzel(filename)
    // Splitting on the empty new line, since the previous is a newline and the line after it was an empty new line this will work well
    val splitContent: Array[String] = content.split("\n\n")
    //    println(splitContent(0))
    //    println(splitContent(1))
    val ruleRaw: Array[String] = splitContent(0).strip().split("\n")
    val graph: mutable.Map[String, mutable.Set[String]] = mutable.Map[String, mutable.Set[String]]()
    val printsSet: mutable.Set[String] = mutable.Set()
    for (line <- ruleRaw) {
      val splitContent: Array[String] = line.split(raw"\|")
      //      println(splitContent.mkString("Array(", ", ", ")"))
      val preq: String = splitContent(0)
      val current: String = splitContent(1)
      val value: mutable.Set[String] = graph.getOrElse(preq, mutable.Set())
      value.add(current)
      graph.put(preq, value)
      printsSet.add(preq)
      printsSet.add(current)
    }

    for ((key, value) <- graph) {
      value.add(Integer.MAX_VALUE.toString)
    }

    graph.put(Integer.MAX_VALUE.toString, mutable.Set())

    var output: Int = 0
    // Going through the possible print order
    val answers: Array[String] = splitContent(1).split("\n")
    for (lines <- answers) {
      var ordering: Array[String] = lines.split(",")
      var queue: mutable.Queue[String] = mutable.Queue()
      val visited: mutable.Set[String] = mutable.Set()
      val root: String = ordering(0)
      // enqueuing node
      queue.enqueue(root)

      var expected: Int = 1
      ordering = ordering :+ Integer.MAX_VALUE.toString

//      println(s"Expected: ${ordering.mkString("Array(", ", ", ")")}")
      while (queue.nonEmpty) {
        val current: String = queue.dequeue()
        if (expected == ordering.length) {
          queue = mutable.Queue()
        }
        val nodes: mutable.Set[String] = graph.getOrElse(current, mutable.Set())
        if (!visited.contains(ordering(expected)) && nodes.contains(ordering(expected)) && expected < ordering.length - 1) {
          queue.enqueue(ordering(expected))
          expected = expected + 1
        }

      }
//      println(s"Final: $expected")
//      println(s"Suppose to be: ${ordering.length - 1}")
      if (expected != ordering.length - 1) {
        var path: List[String] = List()
        for (node <- ordering) {
//          println(ordering.slice(0, ordering.length - 1).mkString("Array(", ", ", ")"))
          val cpath:List[String] = this._createPath(node, ordering.slice(0, ordering.length - 1), graph)
//          println(cpath)
          if (path.isEmpty || cpath.length > path.length){
            path = cpath
          }
        }
        output = output + path(path.length / 2).toInt
      }
    }
    println(output)
  }
}
