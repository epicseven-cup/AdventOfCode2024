package Day4

import Day4.Direction.{Down, Left_Diagonal, Top}

import java.util
import scala.collection.mutable


val XMAS = "XMAS"

enum Direction {
  case Left, Right, Top, Down, Left_Diagonal, Right_Diagonal, Right_Right_Diagonal, Left_Right_Diagonal
}

enum X_MAS {
  case Left_Diagonal, Right_Diagonal
}

val MAS = "MAS"


class Board(val height: Int, val width: Int, val graph: mutable.Map[(Int, Int), Char], val search: String) {
  private val paths: mutable.Set[Array[(Int, Int)]] = mutable.Set()

  def minesweeper(): Int = {
    var count: Int = 0

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        //        println(s"X: ${x}, Y:${y}")
        //        println(count)
        val left: Int = if (this._matcher(x, y, Direction.Left)) 1 else 0
        val right: Int = if (this._matcher(x, y, Direction.Right)) 1 else 0
        val top: Int = if (this._matcher(x, y, Direction.Top)) 1 else 0
        val down: Int = if (this._matcher(x, y, Direction.Down)) 1 else 0
        val leftDiagonal: Int = if (this._matcher(x, y, Direction.Left_Diagonal)) 1 else 0
        val rightDiagonal: Int = if (this._matcher(x, y, Direction.Right_Diagonal)) 1 else 0
        val rightRightDiagonal: Int = if (this._matcher(x, y, Direction.Right_Right_Diagonal)) 1 else 0
        val leftRightDiagonal: Int = if (this._matcher(x, y, Direction.Left_Right_Diagonal)) 1 else 0
        count = count + left + right + top + down + leftDiagonal + rightDiagonal + rightRightDiagonal + leftRightDiagonal
      }
    }
    count
  }

  private def _sweeper(x: Int, y: Int, func: (Int, Int, Int) => (Int, Int)): String = {
    var content: String = ""
    val routes: Array[(Int, Int)] = Array.fill(this.search.length)((0, 0))
    for (i <- 0 until this.search.length) {
      val key: (Int, Int) = func(x, y, i)
      routes(i) = key
      content += this.graph.getOrElse(key, "")
    }


    if (this.paths.contains(routes)) {
      "THIS IS WRONG BRO"
    } else {
      this.paths.add(routes)
      content
    }
  }

  private def _matcher(x: Int, y: Int, direction: Direction): Boolean = {
    direction match {
      case left if direction == Direction.Left => {
        val leftF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis - i, yAxis)
        }
        this._sweeper(x, y, leftF) == this.search
      }
      case right if direction == Direction.Right => {
        val rightF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis + i, yAxis)
        }
        this._sweeper(x, y, rightF) == this.search
      }
      case top if direction == Direction.Top => {
        val topF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis, yAxis - i)
        }
        this._sweeper(x, y, topF) == this.search
      }
      case down if direction == Direction.Down => {
        val downF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis, yAxis + i)
        }
        this._sweeper(x, y, downF) == this.search
      }
      case left_diagonal if direction == Direction.Left_Diagonal => {
        val leftDiagonalF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis - i, yAxis - i)
        }
        this._sweeper(x, y, leftDiagonalF) == this.search
      }
      case right_diagonal if direction == Direction.Right_Diagonal => {
        val rightDiagonalF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis + i, yAxis + i)
        }
        this._sweeper(x, y, rightDiagonalF) == this.search
      }
      case right_right_diagonal if direction == Direction.Right_Right_Diagonal => {
        val rightRightDiagonalF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis + i, yAxis - i)
        }
        this._sweeper(x, y, rightRightDiagonalF) == this.search
      }
      case left_right_diagonal if direction == Direction.Left_Right_Diagonal => {
        val leftRightDiagonalF: (Int, Int, Int) => (Int, Int) = (xAxis: Int, yAxis: Int, i: Int) => {
          (xAxis - i, yAxis + i)
        }
        this._sweeper(x, y, leftRightDiagonalF) == this.search
      }

      case _: Day4.Direction => {
        println("Nothing match")
        false
      }
    }
  }

  private def _crossword(x: Int, y: Int): Boolean = {
    val leftDown: Boolean = this.graph.getOrElse((x - 1, y - 1), 'I').toString + this.graph.getOrElse((x, y), 'N') + this.graph.getOrElse((x + 1, y + 1), 'A') == MAS || (this.graph.getOrElse((x - 1, y - 1), 'I').toString + this.graph.getOrElse((x, y), 'N') + this.graph.getOrElse((x + 1, y + 1), 'A')).reverse == MAS

    val rightUp: Boolean = (this.graph.getOrElse((x + 1, y - 1), 'I').toString + this.graph.getOrElse((x, y), 'N') + this.graph.getOrElse((x - 1, y + 1), 'A') == MAS ) || (this.graph.getOrElse((x + 1, y - 1), 'I').toString + this.graph.getOrElse((x, y), 'N') + this.graph.getOrElse((x - 1, y + 1), 'A')).reverse == MAS

    println((this.graph.getOrElse((x - 1, y - 1), 'I').toString + this.graph.getOrElse((x, y), 'N') + this.graph.getOrElse((x + 1, y + 1), 'A')))
    println((this.graph.getOrElse((x + 1, y - 1), 'I').toString + this.graph.getOrElse((x, y), 'N') + this.graph.getOrElse((x - 1, y + 1), 'A')))
    println(leftDown && rightUp)


    leftDown && rightUp
  }


  def crosswordChecker(): Int = {
    var count:Int = 0
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val current:Int = if (this._crossword(x, y)) 1 else 0
        count += current
      }
    }
    count
  }
}


class Day4 extends Solution {
  override def ProblemOne(filename: String): Unit = {
    val lines: Seq[String] = ReadPuzzle(filename)
    val graph: mutable.Map[(Int, Int), Char] = mutable.Map()
    val row = lines.length
    val col = lines.head.split("").length
    // Creating the graph
    for (y <- lines.indices) {
      val splitContent: Array[Char] = lines(y).toCharArray
      for (x <- splitContent.indices) {
        val content: Char = splitContent(x)
        val id: (Int, Int) = (x, y)
        graph(id) = content
      }
    }

    val board: Board = new Board(row, col, graph, XMAS)
    println(board.minesweeper())
  }

  override def ProblemTwo(filename: String): Unit = {
    val lines: Seq[String] = ReadPuzzle(filename)
    val graph: mutable.Map[(Int, Int), Char] = mutable.Map()
    val row = lines.length
    val col = lines.head.split("").length
    // Creating the graph
    for (y <- lines.indices) {
      val splitContent: Array[Char] = lines(y).toCharArray
      for (x <- splitContent.indices) {
        val content: Char = splitContent(x)
        val id: (Int, Int) = (x, y)
        graph(id) = content
      }
    }

    val board: Board = new Board(row, col, graph, XMAS)
    println(board.crosswordChecker())
  }
}
