package us.nuernberg.hackerrank.lexiorder

object Solution {

  def main(args: Array[String]): Unit = {
    Input.read().foreach {
      w =>
        println(w.partition.map(_.swap).getOrElse("no answer"))
    }
  }
}

object Input {
  def read(src: io.Source = io.Source.stdin): Iterator[Word] = {
    src.getLines.drop(1).map(s => Word(s.toList))
  }
}

final case class Word(s: List[Char]) {

  lazy val partition: Option[Partitioning] = {

    def aux(rest: List[Char], acc: List[Char] = Nil): Option[Partitioning] =
      rest match {
        case Nil => None
        case h0 :: t0 =>
          t0 match {
            case Nil => None
            case h1 :: t1 =>
              if (h0 > h1) {
                Some(Partitioning(t1.reverse, h1, h0 :: acc))
              } else {
                aux(t0, h0 :: acc)
              }
          }
      }

    aux(s.reverse)
  }
}

final case class Partitioning(prefix: List[Char], pivot: Char, suffix: List[Char]) {

  lazy val swap: Partitioning = {

    def aux(rest: List[Char], acc: List[Char] = Nil): Partitioning =
      rest match {
        case Nil => this
        case h :: t =>
          if (h > pivot) {
            Partitioning(prefix, h, acc.reverse ++ (pivot :: t))
          } else {
            aux(t, h :: acc)
          }
      }

    aux(suffix.reverse)
  }

  override val toString: String = s"""${prefix.mkString}$pivot${suffix.mkString}"""

}

object Test {

  val input0 =
    """5
      |0125330
      |ab
      |bb
      |hefg
      |dhck
      |dkhc""".stripMargin

  def main(args: Array[String]): Unit = {
    val inputs = Input.read(src = io.Source.fromString(input0))
    inputs.foreach{
      w =>
        val p = w.partition
        val s = p.map(_.swap)
        println(s.getOrElse("no answer"))
    }
  }
}
