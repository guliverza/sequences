
object App {
  type Upstairs = PartialFunction[List[Long], Long]
  type Downstairs = Function[List[Long], Long]
  type Floor = (List[Long], Downstairs)

  val Minus: Upstairs = { case a :: b :: z => b - a }
  val Plus = new Downstairs {
    def apply(a: List[Long]) = a.head + a.last
    override def toString(): String = "+"
  }

  val Div: Upstairs = { case a :: b :: z if a != 0 && b % a == 0 => b / a }
  val Mult = new Downstairs {
    def apply(x: List[Long]) = x.head * x.last
    override def toString(): String = "*"
  }

  val DivReverse: Upstairs = { case a :: b :: z if b != 0 && a % b == 0 => a / b }
  val MultReverse = new Downstairs {
    def apply(x: List[Long]) = x.last / x.head

    override def toString(): String = "/"
  }

  val FiboUp: Upstairs = { case a :: b :: c :: Nil => a * c - b * b }
  val FiboDown = new Downstairs {
    override def apply(x: List[Long]): Long = x match {
      case n :: a :: b :: Nil => if (a == 0) n + b else (n + b * b) / a
    }

    override def toString(): String = "fibbo"
  }

  val Bricks: Seq[(Upstairs, Downstairs)] = Seq(Minus -> Plus, Div -> Mult, DivReverse -> MultReverse, FiboUp -> FiboDown)

  def main(args: Array[String]) {
    println(calcNext((args map (_.toLong)).toList))
  }

  def calcNext(sequence: List[Long]): Option[Long] = {
    val pyramid = build(sequence, Nil)
    pyramid map { pyramid =>
      val res = pyramid.reverse
      res foreach println
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
        new PartialFunction[(Upstairs, Downstairs), Option[Seq[Floor]]] {
          var nextResult: Option[Seq[Floor]] = None

          override def isDefinedAt(floor: (Upstairs, Downstairs)): Boolean = {
            val (upstairs, downstairs) = floor
            val params = (seq.tails collect {
              case a :: b :: c => List(a, b) ++ c.headOption
            }).toList
            if (params.init.forall(upstairs.isDefinedAt)) {
              val up = params collect upstairs
              nextResult = build(up, result :+ (up -> downstairs))
            }
            nextResult.isDefined
          }

          override def apply(v1: (Upstairs, Downstairs)): Option[Seq[Floor]] = nextResult
        }
      }
      pyramid.flatten.sortBy(_.size).headOption

    }
  }

}
