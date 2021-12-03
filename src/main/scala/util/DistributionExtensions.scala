package util


import org.apache.commons.math3.distribution.{BinomialDistribution, ExponentialDistribution, GammaDistribution, GeometricDistribution, GumbelDistribution, NormalDistribution, PoissonDistribution, UniformRealDistribution}


/**
 * GOAL: have a supertype Distribution[T] and also the functionality of all distributions (cdf, pdf, sample, ...)
 */
object DistributionExtensions {

	trait CDF[T] {
		def cdf(x: T): Double
	}
	trait Distribution[T, D] {
		//val cdf: CDF[T]
		def cdf(x: T): Double // TODO study typeclasses work better in linalg proj to see if need to pass the
		// dist here? cdf(dist, type)
	}
	trait ContinuousDistribution[D] extends Distribution[Double, D]
	trait DiscreteDistribution[D] extends Distribution[Int, D]

	// TODO why not seeing that I implement the cdf method? study linalg typeclasses better
	case class PoissonDist(lambda: Double) extends PoissonDistribution(lambda)
		with ContinuousDistribution[PoissonDist]
	case class BinomialDist(numTrials: Int, p: Double) extends BinomialDistribution(numTrials, p)
		with	DiscreteDistribution
	case class GeometricDist(p: Double) extends GeometricDistribution(p)
		with	DiscreteDistribution

	case class GammaDist(shape: Double, scale: Double) extends GammaDistribution(shape, scale)
		with ContinuousDistribution
	case class NormalDist(mu: Double, std: Double) extends NormalDistribution(mu, std)
		with ContinuousDistribution
	case class ExponentialDist(mean: Double) extends ExponentialDistribution(mean)
		with ContinuousDistribution
	case class UniformContinDist(a: Double, b: Double) extends UniformRealDistribution(a, b)
		with ContinuousDistribution
	case class GumbelDist(mu: Double, beta: Double) extends GumbelDistribution(mu, beta)
		with ContinuousDistribution


	implicit def poissonDist(lambda: Double): Distribution[Int, PoissonDist] = new Distribution[Int, PoissonDist] {
		def cdf(x: Int): Double = PoissonDist(lambda).cumulativeProbability(x)
	}

	implicit def poissonCDF(lambda: Double): CDF[PoissonDist] = new CDF[PoissonDist] {
		def cdf(x: Int): Double = PoissonDist(lambda).cumulativeProbability(x)
	}

	implicit val studentPrinter: Printer[StudentId] = new Printer[StudentId] {
		def getString(a: StudentId): String = s"StudentId: ${a.id}"
	}

	// TODO left off here - how to structure this so that
	// 1) can pa in a distribution when saying Dist[T]
	// 2) can call .cdf() on any Dist[T]?


}
