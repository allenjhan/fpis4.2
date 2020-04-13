object VarianceTest extends App {

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(ys: Seq[Double]) = {
      val sum = ys match {
        case Seq() => None
        case zs => Some(zs.sum)
      }
      sum.map(x => x / xs.size)
    }
    val avg = mean(xs)
    avg.flatMap(x => mean(xs.map(y => math.pow(y - x, 2))))
  }

  println(variance(Seq(1,2,3,4,5)))

}
