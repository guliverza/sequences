import scala.util.Try


object App {
  type Func = PartialFunction[List[Long], Long]
  type Comp = (Long) => Long

  val minus: Func = { case a :: b :: z => b - a }
  val plus: Func = { case a :: b :: z => a + (z.headOption getOrElse b) }
  val div: Func = { case a :: b :: z if a != 0 && b % a == 0 => b / a }
  val mult: Func = { case a :: b :: z => a * (z.headOption getOrElse b) }
  val fiboUp: Func = { case a :: b :: c :: Nil => a * c - b*b }
  val fiboDown: Func = { case n :: a :: b :: Nil => (n + b*b)/a }

  val Bricks: Seq[(Func, Func)] = Seq(minus -> plus, div -> mult, fiboUp -> fiboDown)

  def main(args: Array[String]) {
    val sequence = (args map (_.toLong)).toList
    val pyramid = build(sequence, Nil) orElse build(sequence.reverse, Nil)
    pyramid.fold(println("Nothing found")) { pyramid =>
      val res = pyramid.reverse
      println(res)
      val result = if (pyramid.isEmpty)
        sequence.last
      else {
        val (num, lastOp) = res.tail.foldLeft(res.head._1.last -> res.head._2) {
          case ((floor, f), (nextNum, nextF)) =>
            (f(floor +: nextNum.takeRight(2)), nextF)
        }
        lastOp(num +: sequence.takeRight(2))
      }
      println(result)
    }
  }

  def build(seq: Seq[Long], result: Seq[(List[Long], Func)]): Option[Seq[(List[Long], Func)]] = {
    if (seq.size < 2) None
    else if (seq.toSet.size == 1) Some(result)
    else {
      val pyramid = Bricks collect {
        new PartialFunction[(Func, Func), Option[Seq[(List[Long], Func)]]] {
          var nextResult: Option[Seq[(List[Long], Func)]] = None

          override def isDefinedAt(floor: (Func, Func)): Boolean = {
            val (upstairs, downstairs) = floor
            val up = seq.tails collect {
              case a :: b :: c => List(a, b) ++ c.headOption
            } collect upstairs
            val upList = up.toList
            if (seq.size - upList.size <= 2) {
              nextResult = build(upList, result :+ (upList -> downstairs))
            }
            nextResult.isDefined
          }

          override def apply(v1: (Func, Func)): Option[Seq[(List[Long], Func)]] = nextResult
        }
      }
      pyramid.flatten.sortBy(_.size).headOption

    }
  }

}
