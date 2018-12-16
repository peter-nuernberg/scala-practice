package us.nuernberg.hackerrank.magicsquare

object Solution {

  def main(args: Array[String]): Unit = {
    val input: Square = Solution.read()
    println(Square.allMagics.map { m => m - input }.min)
  }

  def read(src: io.Source = io.Source.stdin): Square =
    Square(src.getLines().foldLeft(List.empty[Int]) {
      case (acc, line) => acc ++ line.split(' ').toList.map(_.toInt)
    })
}

/*

   ORIG       FLIP
   2 7 6      4 3 8
   9 5 1      9 5 1
   4 3 8      2 7 6

   ROTATE
   4 9 2      8 1 6
   3 5 7      3 5 7
   8 1 6      4 9 2

   8 3 4      6 7 2
   1 5 9      1 5 9
   6 7 2      8 3 4

   6 1 8      2 9 4
   7 5 3      7 5 3
   2 9 4      6 1 8
 */

final case class Square(elems: List[Int]) {

  require(elems.length == 9, s"need 9 elements, but got ${elems.length} ($elems)")

  def -(other: Square): Int = elems.zip(other.elems).map { case (a, b) => (a - b).abs }.sum

  lazy val coords: Map[(Int, Int), Int] =
    elems.zipWithIndex.map {
      case (elem, index) =>
        val row = index / 3
        val col = index % 3
        (row, col) -> elem
    }.toMap

  override lazy val toString: String =
    coords.groupBy { case ((r, _), _) => r }
      .toList.sortBy { case (r, _) => r }.map(_._2)
      .map(_.toList.sortBy { case ((_, c), _) => c }.map { case ((_, _), e) => e }.mkString(" ")).mkString("\n")

  /**
   *  Returns the result of rotating this square 90 degrees clockwise.
   *
   *  {{{
   *  (0, 0)  (0, 1)  (0, 2)                   (2, 0)  (1, 0)  (0, 0)
   *  (1, 0)  (1, 1)  (1, 2)   == ROTATE ==>   (2, 1)  (1, 1)  (0, 1)
   *  (2, 0)  (2, 1)  (2, 2)                   (2, 2)  (1, 2)  (0, 2)
   *
   *  (0, 0) => (0, 2)
   *  (0, 1) => (1, 2)
   *  (0, 2) => (2, 2)
   *  (1, 0) => (0, 1)
   *  (1, 1) => (1, 1)
   *  (1, 2) => (2, 1)
   *  (2, 0) => (0, 0)
   *  (2, 1) => (1, 0)
   *  (2, 2) => (2, 0)
   *
   *  (r, c) => (c, 2-r)
   *  }}}
   *
   *  @example{{{
   *  2 7 6                   4 9 2
   *  9 5 1   == ROTATE ==>   3 5 7
   *  4 3 8                   8 1 6
   *  }}}
   */
  lazy val rotate: Square = xform { case (r, c) => (c, 2 - r) }

  /**
   *  Returns the result of flipping this square along the x-axis.
   *
   *  {{{
   *  (0, 0)  (0, 1)  (0, 2)                   (2, 0)  (2, 1)  (2, 2)
   *  (1, 0)  (1, 1)  (1, 2)   == ROTATE ==>   (1, 0)  (1, 1)  (1, 2)
   *  (2, 0)  (2, 1)  (2, 2)                   (0, 0)  (0, 1)  (0, 2)
   *
   *  (0, 0) => (2, 0)
   *  (0, 1) => (2, 1)
   *  (0, 2) => (2, 2)
   *  (1, 0) => (1, 0)
   *  (1, 1) => (1, 1)
   *  (1, 2) => (1, 2)
   *  (2, 0) => (0, 0)
   *  (2, 1) => (0, 1)
   *  (2, 2) => (0, 2)
   *
   *  (r, c) => (2-r, c)
   *  }}}
   *
   *  @example{{{
   *  2 7 6                  4 3 8
   *  9 5 1   == ROTATE ==>  9 5 1
   *  4 3 8                  2 7 6
   *  }}}
   */
  lazy val flip: Square = xform { case (r, c) => (2 - r, c) }

  private def xform(f: (Int, Int) => (Int, Int)): Square =
    Square(coords.map {
      case ((r, c), e) =>
        val (nr, nc) = f(r, c)
        ((nr * 3) + nc) -> e
    }.toList.sortBy(_._1).map(_._2))
}

object Square {

  val magic0: Square = Square(List(2, 7, 6, 9, 5, 1, 4, 3, 8))
  val allMagics: List[Square] =
    List.iterate(magic0, 4) { _.rotate }
      .flatMap { s => List(s, s.flip) }
}

object Test {

  val input0: String = """4 9 2
                         |3 5 7
                         |8 1 5""".stripMargin

  def main(args: Array[String]): Unit = {
    val square0: Square = Solution.read(io.Source.fromString(input0))
    println(Square.allMagics.map { m => m - square0 }.min)
  }
}
