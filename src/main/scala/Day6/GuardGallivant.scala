package Day6

import scala.collection.mutable

val expecting: Array[Char] = Array('v', '<', '^', '>')

val moveMapping: Map[Char, (Int, Int) => (Int, Int)] = Map(
  'v' -> {(x:Int, y:Int) => (x,y + 1)},
  '<' -> {(x:Int, y:Int) => (x - 1,y)},
  '^' -> {(x:Int, y:Int) => (x,y - 1)},
  '>' -> {(x:Int, y:Int) => (x + 1,y)},
)

val wallChar: Char = '#'

class Robot(var location:(Int, Int), var sign:Char, val boardHeight:Int, val boardWidth:Int, val wall:mutable.Set[(Int, Int)]) {
  var pointer: Int = expecting.indexOf(this.sign)
  def move(): Option[Boolean] = {
    val func:Option[(Int, Int) => (Int, Int)] = moveMapping.get(this.sign)
    if(func.nonEmpty){
      val newLocation = func.get.apply(location._1, location._2)
      if (newLocation._1 >= this.boardWidth || newLocation._1 < 0 || newLocation._2 >= this.boardHeight || newLocation._2 < 0){
        return None
      } else if (!wall.contains(newLocation)){
        this.location = newLocation
        return Option(true)
      } else{
        return Option(false)
      }
    }
    println("Robot is misconfigured")
    None
  }


  // Return possible position
  def rotate(): Unit = {
    this.pointer = (this.pointer + 1) % expecting.length
    this.sign = expecting(this.pointer)
  }
}

class GuardGallivant extends Solution {
  override def ProblemOne(filename:String): Unit ={
    var botLocation: (Int, Int) = (-1, -1)
    val lines: Seq[String]= ReadPuzzle(filename)
    val board:Array[Array[Char]]= new Array[Array[Char]](lines.toArray.length)
    val wallSet: mutable.Set[(Int, Int)] = mutable.Set()
    for (y <- lines.indices){
      board(y) = lines(y).toCharArray
      for (x <- board(y).indices){
        if (expecting.contains(board(y)(x))){
          botLocation = (x, y)
        }

        if (board(y)(x) == wallChar){
          wallSet.add((x, y))
        }

      }
    }

    val robot:Robot = new Robot(botLocation, board(botLocation._2)(botLocation._1), board.length, board(0).length, wallSet)
    val visitedBlocks: mutable.Set[(Int, Int)] = mutable.Set()


    var able:Option[Boolean] = Option(true)
    while (able.nonEmpty) {
      visitedBlocks.add(robot.location)
      able = robot.move()
      if ( able.nonEmpty && !able.get) {
        robot.rotate()
      }
    }

    for((x, y) <- visitedBlocks){
      board(y)(x) = 'X'
    }

    board(botLocation._2)(botLocation._1) = '^'

    val path: os.Path = os.pwd / "day6_output.txt"

    var rawBoard:String = ""

    board.foreach( (yLevel: Array[Char]) =>
      rawBoard = rawBoard.appendedAll(yLevel.mkString)
      rawBoard += '\n'
    )

    os.write.over(path, rawBoard)

    println(visitedBlocks.size)
  }
  override def ProblemTwo(filename:String): Unit ={}

}
