import scala.math.{log, pow}

object Util {

  // max
  def max[T](list: List[T])(implicit ord: Ordering[T]): T =
    list.max

  // map
  def map[T, U](list: List[T], f: T => U): List[U] =
    list.map(f)

  // isSorted
  def isSorted[T](list: List[T])(implicit ord: Ordering[T]): Boolean =
    list == list.sorted

  // probs
  def probs[T](list: List[T]): Map[T, Double] = {
    val freqs = list.groupBy(identity).mapValues(_.size)
    val total = list.size.toDouble
    freqs.mapValues(_ / total)
  }

  // entropy
  def entropy[T](list: List[T]): Double = {
    val freqs = probs(list).values
    freqs.map(p => -p * log(p) / log(2)).sum
  }

  // mu
  def mu(list: List[Double]): Double =
    list.sum / list.length.toDouble

  // variance
  def variance(list: List[Double]): Double = {
    val m = mu(list)
    list.map(x => pow(x - m, 2)).sum / (list.length - 1).toDouble
  }

  // zscore
  def zscore(list: List[Double]): List[Double] = {
    val m = mu(list)
    val s = math.sqrt(variance(list))
    list.map(x => (x - m) / s)
  }

  // cov
  def cov(list1: List[Double], list2: List[Double]): Double = {
    val m1 = mu(list1)
    val m2 = mu(list2)
    val n = list1.length.toDouble
    (list1, list2).zipped.map((x, y) => (x - m1) * (y - m2)).sum / (n - 1)
  }

  // pearson
  def pearson(list1: List[Double], list2: List[Double]): Double = {
    val covar = cov(list1, list2)
    val sd1 = math.sqrt(variance(list1))
    val sd2 = math.sqrt(variance(list2))
    covar / (sd1 * sd2)
  }
}
