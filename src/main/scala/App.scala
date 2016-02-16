
object App {
  type Func = PartialFunction[List[Long], Long]
  type Floor = (List[Long], Func)

  val Minus: Func = { case a :: b :: z => b - a }
  val Plus: Func = { case a :: b :: z => a + (z.headOption getOrElse b) }
  val Div: Func = { case a :: b :: z if a != 0 && b % a == 0 => b / a }
  val Mult: Func = { case a :: b :: z => a * (z.headOption getOrElse b) }
  val DivReverse: Func = { case a :: b :: z if b != 0 && a % b == 0 => a / b }
  val MultReverse: Func = { case a :: b :: z => a / (z.headOption getOrElse b) }
  val FiboUp: Func = { case a :: b :: c :: Nil => a * c - b*b }
  val FiboDown: Func = { case n :: a :: b :: Nil => if (a == 0) n + b else (n + b*b)/a }

  val Bricks: Seq[(Func, Func)] = Seq(Minus -> Plus, Div -> Mult, DivReverse -> MultReverse, FiboUp -> FiboDown)

  def main(args: Array[String]) {
    println(calcNext((args map (_.toLong)).toList))
  }

  def calcNext(sequence: List[Long]): Option[Long] = {
    val pyramid = build(sequence, Nil)
    pyramid map { pyramid =>
      val res = pyramid.reverse
      println(res)
      if (pyramid.isEmpty)
        sequence.last
      else {
        val (num, lastOp) = res.tail.foldLeft(res.head._1.last -> res.head._2) {
          case ((floor, f), (nextNum, nextF)) =>
            (f(floor +: nextNum.takeRight(2)), nextF)
        }
        lastOp(num +: sequence.takeRight(2))
      }
    }
  }

  def build(seq: Seq[Long], result: Seq[Floor]): Option[Seq[Floor]] = {
    if (seq.size < 2) None
    else if (seq.toSet.size == 1) Some(result)
    else {
      val pyramid = Bricks collect {
        new PartialFunction[(Func, Func), Option[Seq[Floor]]] {
          var nextResult: Option[Seq[Floor]] = None

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

          override def apply(v1: (Func, Func)): Option[Seq[Floor]] = nextResult
        }
      }
      pyramid.flatten.sortBy(_.size).headOption

    }
  }

}
