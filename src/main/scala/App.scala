import scala.util.Try


object App {
  type Func = (Long, Long) => Long
  type Comp = (Long) => Long


  val minus = new ((Long, Long) => Long) {
    override def apply(a: Long, b: Long): Long = a - b
    override def toString: String = "-"
  }
  val plus = new ((Long, Long) => Long) {
    override def apply(a: Long, b: Long): Long = a + b
    override def toString: String = "+"
  }
  val div = new ((Long, Long) => Long) {
    override def apply(a: Long, b: Long): Long = a / b
    override def toString: String = "/"
  }
  val mult = new ((Long, Long) => Long) {
    override def apply(a: Long, b: Long): Long = a * b
    override def toString: String = "*"
  }

  val Probes: Seq[(Func, Func)] = Seq(minus -> plus, div -> mult)

  def comp(k: Long, f: Func)(n: Long) = f(n, k)

  def main(args: Array[String]) {
    val sequence = args map (_.toLong)
    val r = probe(sequence, Nil) orElse probe(sequence.reverse, Nil)
    r.fold(println("Nothing found")) { r =>
      val res = r.reverse
      println(res)
      val (num, lastOp) = res.tail.foldLeft(res.head) { case ((num, f), (nextNum, nextF)) =>
        (f(nextNum, num), nextF)
      }
      val result = lastOp(num, sequence.last)
      println(result)
    }
  }

  def probe(s: Seq[Long], result: Seq[(Long, Func)]): Option[Seq[(Long, Func)]] = {
    if (s.size < 2) None
    else if (s.toSet.size == 1) Some(result)
    else {
      val probes: Seq[Option[Seq[(Long, Func)]]] = Probes collect {
        new PartialFunction[(Func, Func), Option[Seq[(Long, Func)]]] {
          var nextResult: Option[Seq[(Long, Func)]] = None

          override def isDefinedAt(inOut: (Func, Func)): Boolean = Try {
            val (split, restore) = inOut
            val up = (s.tail zip s.init) map { case (a, b) => split(a, b) }
            nextResult = probe(up, result :+ (up.last -> restore))
            nextResult.isDefined
          } getOrElse false

          override def apply(v1: (Func, Func)): Option[Seq[(Long, Func)]] = nextResult
        }
      }
      probes.flatten.sortBy(_.size).headOption

    }
  }



}
