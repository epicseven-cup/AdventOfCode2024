package scala


abstract class Solution {
  def ProblemOne(filename:String): Unit = {}
  def ProblemTwo(filename:String): Unit = {}
}


def ReadPuzzle(filename:String): Seq[String] =
  val path: os.Path = os.pwd / "src" / "data" / filename
  val content: String = os.read(path)
  val lines: Seq[String] = os.read.lines(path)
  lines
