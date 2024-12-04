package Day2Red_NosedReports

class Solution {
  def ProblemOne(filename: String): Unit = {
    val path: os.Path = os.pwd / "src" / "data" / filename

    val content: String = os.read(path)
    val lines: Seq[String] = os.read.lines(path)

    var result: Int = 0
    lines.foreach((line: String) => {
      var level: Array[Int] = line.split(" ").map((x: String) => {
        x.toInt
      })
      var status: Boolean = true


      // This is looking at if the array is all increase
      for (i <- level.indices) {
        var prev: Boolean = true
        if (i > 0) {
          prev = level(i - 1) < level(i) && math.abs(level(i - 1) - level(i)) <= 3
        }
        status = status && prev
      }

      if (status) {
        result = result + 1
      } else {
        status = true
        for (i <- level.indices) {
          var prev: Boolean = true
          if (i > 0) {
            prev = level(i - 1) > level(i) && math.abs(level(i - 1) - level(i)) <= 3
          }
          status = status && prev
        }

        if (status) {
          result = result + 1
        }
      }
    })

    println(s"Safe: $result")
  }

  def checker(level: Array[Int]): Boolean = {
    var status: Boolean = true


    // This is looking at if the array is all increase
    for (i <- level.indices) {
      var prev: Boolean = true
      if (i > 0) {
        prev = level(i - 1) < level(i) && math.abs(level(i - 1) - level(i)) <= 3
      }
      status = status && prev
    }

    if (status) {
      return status
    } else {
      status = true
      for (i <- level.indices) {
        var prev: Boolean = true
        if (i > 0) {
          prev = level(i - 1) > level(i) && math.abs(level(i - 1) - level(i)) <= 3
        }
        status = status && prev
      }
    }
    status
  }


  def ProblemTwo(filename: String): Unit = {
    val path: os.Path = os.pwd / "src" / "data" / filename

    val content: String = os.read(path)
    val lines: Seq[String] = os.read.lines(path)
    var result: Int = 0

    lines.foreach((line: String) => {
      var level: Array[Int] = line.split(" ").map((x: String) => {
        x.toInt
      })
      var i: Int = 0
//      println(level.mkString("Array(", ", ", ")"))
      while (i < level.length) {
        val input: Array[Int] = level.slice(0, i) ++ level.slice(i + 1, level.length)
//        println(input.mkString("Array(", ", ", ")"))
        val bool:Boolean = this.checker(input)
        if (bool){
          result = result + 1
          i = level.length
        }
        i = i + 1
      }
    })
    println(s"Result: $result")
  }
}
