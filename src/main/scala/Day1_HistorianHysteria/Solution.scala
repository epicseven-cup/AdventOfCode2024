package Day1_HistorianHysteria

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

class Solution {
  def ProblemOne(path:String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    var left, right = new Array[Int](0)
    source.getLines().foreach( (line:String) => {
      val result:Array[String] = line.split("   ")
      left = left :+ result(0).toInt
      right = right :+ result(1).toInt
    })

    left = left.sortBy(x => x)
    right = right.sortBy(x => x)
//    println(left.mkString("Array(", ", ", ")"))
//    println(right)
    var distance = 0
    for (i <- left.indices){
      distance = distance + math.abs(right(i) - left(i))
    }
//    println(distance)
    source.close()
  }

  def ProblemTwo(path:String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val right: mutable.Map[Int, Int] = mutable.Map()
    var appear: Array[Int] = Array()
    source.getLines().foreach((line: String) => {
      val result: Array[String] = line.split("   ")

      val rightValue:Int = right.getOrElse(result(1).toInt, 0) + 1

      right(result(1).toInt) = rightValue


      // Adding it to set
      appear = appear :+ result(0).toInt
    })
    println(appear.mkString("Array(", ", ", ")"))
    var similarity:Int = 0
    for ( n <- appear){
      val rightValue:Int = right.getOrElse(n, 0)

      similarity += ( n* rightValue)
    }

    println(similarity)
  }


}
