package Day3MullItOver

import scala.util.matching.Regex

class MullItOver extends Solution {
  override def ProblemOne(filename:String): Unit = {
    val lines:Seq[String] = ReadPuzzle(filename)
    var total:Int = 0
    lines.foreach( (line:String) => {
      val mulMatching: Regex = raw"mul\([0-9]+,[0-9]+\)".r
      val matched: Iterator[Regex.Match] = mulMatching.findAllMatchIn(line)
      matched.foreach( (s:Regex.Match) => {
        val replace:Array[Int] = s.toString().replace("mul", "").replace("(", "").replace(")", "").split(",").map( (n:String) => n.toInt)
        total = total + (replace(0) * replace(1))
      })

    })
    println(total)
  }

  override def ProblemTwo(filename: String): Unit =  {
    val lines:Seq[String] = ReadPuzzle(filename)
    val mulMatching: Regex = raw"mul\([0-9]+,[0-9]+\)".r
    val doNotMatching:Regex = raw"don't\(\).*".r
    val doMatching:Regex = raw"do\(\)".r
    var total:Int = 0
    var input:String = ""
    lines.foreach( (s:String) => {
      input += s
    })
    val doLines: Array[String] = doMatching.split(input)
    for (i <- doLines) {
      val clean: String = doNotMatching.replaceAllIn(i, "")
      mulMatching.findAllMatchIn(clean).foreach((s: Regex.Match) => {
        val replace: Array[Int] = s.toString().replace("mul", "").replace("(", "").replace(")", "").split(",").map((n: String) => n.toInt)
        total = total + (replace(0) * replace(1))
      })
    }
    println(total)
  }
}
