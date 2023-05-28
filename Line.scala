class Line(ps:Array[Point]) {

	// read only values a and b

	// f

	// dist
}
class Line(points: Array[Point]) {
  require(points.length >= 2, "At least 2 points are required to define a line")

  // calculate slope and y-intercept
  private val xValues = points.map(_.x)
  private val yValues = points.map(_.y)
  private val slope = Util.cov(xValues, yValues) / Util.variance(xValues)
  private val yIntercept = Util.mu(yValues) - slope * Util.mu(xValues)

  // equation of the line
  def f(x: Double): Double = slope * x + yIntercept

  // variance of y-values
  def variance(): Double = Util.variance(yValues)

  // covariance of x and y values
  def cov(): Double = Util.cov(xValues, yValues)

  // mean of x values
  def mu_x(): Double = Util.mu(xValues)

  // mean of y values
  def mu_y(): Double = Util.mu(yValues)

  def distance(p: Point): Double = {
    val numerator = math.abs((points(1).y - points(0).y) * p.x - (points(1).x - points(0).x) * p.y + points(1).x * points(0).y - points(1).y * points(0).x)
    val denominator = math.sqrt(math.pow(points(1).y - points(0).y, 2) + math.pow(points(1).x - points(0).x, 2))
    numerator / denominator
  }
}
