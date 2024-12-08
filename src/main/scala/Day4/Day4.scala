package Day4

import java.util
import scala.collection.mutable


val XMAS = "XMAS"


case class XMASNode(content:Char, id:(Int, Int)){}
/**
 *  Used to carry the current search data
 * @param payload, content of the current string
 * @param visited, all the nodes' id visited
 * @param index, index of the XMAS string that it is looking for
 */
class Payload(val payload:String, var lastId:(Int, Int), var index:Int, var path:Set[(Int, Int)]){}


class Day4 extends Solution {
  override def ProblemOne(filename: String): Unit = {
    val lines: Seq[String] = ReadPuzzle(filename)
    val graph:mutable.Map[(Int, Int), XMASNode] = mutable.Map()
    val row = lines.length
    val col = lines.head.split("").length
    // Creating the graph
    for (y <- lines.indices) {
      val splitContent:Array[Char] = lines(y).toCharArray
      for (x <- splitContent.indices){
        val content:Char = splitContent(x)
        val id:(Int, Int)= (x, y)
        graph(id) = XMASNode(content = content, id = id)
      }
    }

    var count:Int = 0
    var visitedPath:mutable.Set[Set[(Int, Int)]] = mutable.Set()
    // Going through the option
    for (y <- 0 until row) {
      for (x <- 0 until col) {
        val rootNode: XMASNode = graph((x, y))
        if (rootNode.content == XMAS(0)) {
          val rootPayload: Payload = new Payload(payload = "" + rootNode.content, lastId=rootNode.id, index = 0, path = Set(rootNode.id))
          val queue: mutable.Queue[Payload] = new mutable.Queue[Payload]()
          // Enqueuing and adding it as visted
          queue.enqueue(rootPayload)

          // Visited nodes
          val visited:mutable.Set[(Int, Int)] = mutable.Set()
          visited += rootNode.id

          // BFS search as long as it is not empty
          while (queue.nonEmpty) {
            val currentPayload:Payload = queue.dequeue()
            println(currentPayload.path)
            println(currentPayload.payload)
            if (currentPayload.payload == XMAS && !visited.contains(currentPayload.lastId) && !visitedPath.contains(currentPayload.path)){
              visitedPath.add(currentPayload.path)
              count += 1
            } else {
              // Up
              val upNode: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1, currentPayload.lastId._2 - 1), null))
              if (currentPayload.payload.length < XMAS.length && upNode.nonEmpty && upNode.get.content == XMAS(currentPayload.index + 1) && !visited.contains(upNode.get.id)) {
                // Keep track of all visited nodes and enqueue the nodes into current queue
                val upPayload: Payload = new Payload(payload = currentPayload.payload + upNode.get.content, lastId = upNode.get.id, index = currentPayload.index + 1, path = currentPayload.path + upNode.get.id)
                queue.enqueue(upPayload)
              }


              // Down
              val downNode: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1, currentPayload.lastId._2 + 1), null))
              if (currentPayload.payload.length < XMAS.length && downNode.nonEmpty && downNode.get.content == XMAS(currentPayload.index + 1) && !visited.contains(downNode.get.id)) {
                // Keep track of all visited nodes and enqueue the nodes into current queue
                val downPayload: Payload = new Payload(payload = currentPayload.payload + downNode.get.content, lastId = downNode.get.id, index = currentPayload.index + 1, path = currentPayload.path + downNode.get.id)
                queue.enqueue(downPayload)
              }


              // Left
              val leftNode: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1 - 1, currentPayload.lastId._2), null))
              if (currentPayload.payload.length < XMAS.length && leftNode.nonEmpty && leftNode.get.content == XMAS(currentPayload.index + 1) && !visited.contains(leftNode.get.id)) {
                // Keep track of all visited nodes and enqueue the nodes into current queue
                val leftPayload: Payload = new Payload(payload = currentPayload.payload + leftNode.get.content, lastId = leftNode.get.id, index = currentPayload.index + 1, path = currentPayload.path + leftNode.get.id)
                queue.enqueue(leftPayload)
              }


              // Right
              val rightNode: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1 + 1, currentPayload.lastId._2), null))
              if (currentPayload.payload.length < XMAS.length && rightNode.nonEmpty && rightNode.get.content == XMAS(currentPayload.index + 1) && !visited.contains(rightNode.get.id)) {
                val rightPayload: Payload = new Payload(payload = currentPayload.payload + rightNode.get.content, lastId = rightNode.get.id, index = currentPayload.index + 1, path = currentPayload.path + rightNode.get.id)
                queue.enqueue(rightPayload)
              }


              // Left Up Diagonal
              val leftUpDiagonal: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1 - 1, currentPayload.lastId._2 - 1), null))
              if (currentPayload.payload.length < XMAS.length && leftUpDiagonal.nonEmpty && leftUpDiagonal.get.content == XMAS(currentPayload.index + 1) && !visited.contains(leftUpDiagonal.get.id)) {
                val  leftUpDiagonalPayload = new Payload(payload = currentPayload.payload + leftUpDiagonal.get.content, lastId = leftUpDiagonal.get.id, index = currentPayload.index + 1, path = currentPayload.path + leftUpDiagonal.get.id)
                queue.enqueue(leftUpDiagonalPayload)
              }

              // Right Up Diagonal
              val rightUpDiagonal: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1 - 1, currentPayload.lastId._2 + 1), null))
              if (currentPayload.payload.length < XMAS.length && rightUpDiagonal.nonEmpty && rightUpDiagonal.get.content == XMAS(currentPayload.index + 1) && !visited.contains(rightUpDiagonal.get.id)) {
                val rightUpDiagonalPayload: Payload = new Payload(payload = currentPayload.payload + rightUpDiagonal.get.content, lastId = rightUpDiagonal.get.id, index = currentPayload.index + 1, path = currentPayload.path + rightUpDiagonal.get.id)
                queue.enqueue(rightUpDiagonalPayload)
              }

              // Left Down Diagonal
              val leftDownDiagonal: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1 + 1, currentPayload.lastId._2 - 1), null))
              if (currentPayload.payload.length < XMAS.length && leftDownDiagonal.nonEmpty && leftDownDiagonal.get.content == XMAS(currentPayload.index + 1) && !visited.contains(leftDownDiagonal.get.id)) {
                val leftDownDiagonalPayload: Payload = new Payload(payload = currentPayload.payload + leftDownDiagonal.get.content, lastId = leftDownDiagonal.get.id, index = currentPayload.index + 1, path = currentPayload.path + leftDownDiagonal.get.id)
                queue.enqueue(leftDownDiagonalPayload)
              }


              // Right Down Diagonal
              val rightDownDiagonal: Option[XMASNode] = Option(graph.getOrElse((currentPayload.lastId._1 + 1, currentPayload.lastId._2 + 1), null))
              if (currentPayload.payload.length < XMAS.length && rightDownDiagonal.nonEmpty && rightDownDiagonal.get.content == XMAS(currentPayload.index + 1) && !visited.contains(rightDownDiagonal.get.id)) {
                val rightDownDiagonalPayload: Payload = new Payload(payload = currentPayload.payload + rightDownDiagonal.get.content, lastId = rightDownDiagonal.get.id, index = currentPayload.index + 1, path = currentPayload.path + rightDownDiagonal.get.id)
                queue.enqueue(rightDownDiagonalPayload)
              }


            }

            visited += currentPayload.lastId
          }
        }
      }
    }
    println(count)
  }

  override def ProblemTwo(filename: String): Unit =  {
    val lines: Seq[String] = ReadPuzzle(filename)

  }
}
