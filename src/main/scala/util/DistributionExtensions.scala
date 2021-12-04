package util


import org.apache.commons.math3.distribution.{BinomialDistribution, ExponentialDistribution, GammaDistribution, GeometricDistribution, GumbelDistribution, NormalDistribution, PoissonDistribution, UniformRealDistribution}


/**
 * GOAL: have a supertype Distribution[T] and also the functionality of all distributions (cdf, pdf, sample, ...)
 */
object DistributionExtensions {

	trait CDF[T, D] {
		def getDistFromCDFArea(d: D): D // NOTE: this functions not necessary for the experiment of making
		// AbsDist[T] callable with .cdf
		def cumulativeProbability(d: D, x: T): Double
	}
	trait AbsDist[T, D] /*extends CDF[T, AbsDist[T, D]]*/{
		def getDist: D
	}

	trait Distribution[T]  {
		def cdf(x: T): Double
	}

	//trait ContinuousDistribution[D] extends Distribution[Double, D]
	//trait DiscreteDistribution[D] extends Distribution[Int, D]

	// TODO why not seeing that I implement the cdf method? study linalg typeclasses better
	//trait Pois extends ContinuousDistribution[Pois]
	case class PoissonDist(lambda: Double) extends PoissonDistribution(lambda)
		with Distribution[Int] {

		override def cdf(x: Int): Double = new PoissonDistribution(lambda).cumulativeProbability(x)
	}

	case class PoisAbsDist(lambda: Double) extends PoissonDistribution(lambda)	with AbsDist[Int, PoisAbsDist]
	{ def getDist: PoisAbsDist = this }
	/*case class BinomialDist(numTrials: Int, p: Double) extends BinomialDistribution(numTrials, p)
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
		with ContinuousDistribution*/


	// NOTE: put the cdf() in the distribution trait, and just use syntax to call the .cdf() on each distribution[T]
	//  type.
	/*object Pois {

		/*implicit def poissonDist(lambda: Double): AbstractDistribution[Int] = (x: Int) => new PoissonDistribution(lambda)
			.cumulativeProbability(x)*/
		implicit def poissonCDF(lambda: Double): CDF[Int] = new CDF[Int] {
			def cdf(x: Int): Double = new PoissonDistribution(lambda).cumulativeProbability(x)
		}
	}*/



	/*implicit class DistOps[T: Numeric, D](current: Distribution[T])(implicit ev: CDF[T]) {
		def cdf(x: T): Double = ev.cdf(x)
	}*/
	/*implicit def poissonCDF(lambda: Double): CDF[Pois] = new CDF[Pois] {
		def cdf(x: Int): Double = Pois(lambda).cumulativeProbability(x)
	}

	implicit val studentPrinter: Printer[StudentId] = new Printer[StudentId] {
		def getString(a: StudentId): String = s"StudentId: ${a.id}"
	}*/

	// TODO left off here - how to structure this so that
	// 1) can pa in a distribution when saying Dist[T]
	// 2) can call .cdf() on any Dist[T]?



	/*trait DistSyntax {
		implicit class DistOps[T: Numeric, D](current: D)(implicit ev: Distribution[T, D]){
			def cdf(x: T): Double = ev.cumulProb(current, x)
		}
	}*/

	/*abstract class PoissonIsDistribution(lambda: Double) extends PoissonDistribution(lambda) with AbsDist[Int,
		PoissonDistribution]

	class PoissonHasCDF extends CDF[Int, PoissonDistribution] {
		def cumulativeProbability(d: PoissonDistribution, x: Int): Double = d.cumulativeProbability(x)
	}*/
	object PoissonCDF {
		implicit def poissonHasCDF = new CDF[Int, PoisAbsDist] {
			def cumulativeProbability(d: PoisAbsDist, x: Int): Double = d.cumulativeProbability(x)

			def getDistFromCDFArea(d: PoisAbsDist): PoisAbsDist = d
		}
	}
	import PoissonCDF._
	/*class PoissonHasCDF extends CDF[Int, PoisAbsDist] {
		def cumulativeProbability(d: PoisAbsDist, x: Int): Double = d.cumulativeProbability(x)
	}
	val v = new PoissonHasCDF */ // TODO look in linalg proj to see if you can make it work this way too



	implicit class CDFSyntax[T: Numeric, D](current: D)(implicit ev: CDF[T, D]){
		def cdf(x: T): Double = ev.cumulativeProbability(ev.getDistFromCDFArea(current), x)
	}
	// TODO why can't call .cdf now for PoisAbsDist(n) ? -- .absdistCDF works
	// TODO is this class even necessary?
	//PoisAbsDist(1.2)

	implicit class AbsDistSyntax[T: Numeric, D](absdist: AbsDist[T, D])(implicit ev: CDF[T, D]) {
		def absdistCDF(x: T): Double = ev.cumulativeProbability(absdist.getDist, x)
	}
	// --- .absdistCDF works
	//PoisAbsDist(1.2).absdistCDF(3)


	def testUsage[T: Numeric, D](x: T, distAbs: AbsDist[T, D])(implicit ev: CDF[T, D]): Double = {
		distAbs.absdistCDF(x)

	}

	// ugly way = with Distribution[T] having the cdf inside which unfortunately makes PoisDist have to re-implement
	// it inside its class again (totally ugly)
	def testUsageOfCDF_uglyway[T: Numeric](x: T, distT: Distribution[T]): Double = {
		distT.cdf(x)
	}

	def main(args: Array[String]) {
		Console.println(testUsageOfCDF_uglyway[Int](10, PoissonDist(3.4)))

		Console.println(testUsage[Int, PoisAbsDist](10, PoisAbsDist(3.4)))
	}

}