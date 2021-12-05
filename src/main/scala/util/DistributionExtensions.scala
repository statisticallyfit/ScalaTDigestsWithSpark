package util


import org.apache.commons.math3.distribution.{BinomialDistribution, ExponentialDistribution, GammaDistribution, GeometricDistribution, GumbelDistribution, NormalDistribution, PoissonDistribution, UniformRealDistribution}


/**
 * GOAL: have a supertype Distribution[T] and also the functionality of all distributions (cdf, pdf, sample, ...)
 */
object DistributionExtensions {

	trait CDF[T, D] {
		//def getDistFromCDFArea(d: D): D // NOTE: this functions not necessary for the experiment of making
		// AbsDist[T] callable with .cdf
		def cumulativeProbability(d: D, x: T): Double
	}
	trait Dist[T, D] /*extends CDF[T, AbsDist[T, D]]*/{
		def getDist: D
	}
	trait ContinuousDist[D] extends Dist[Double, D]
	trait DiscreteDist[D] extends Dist[Int, D]


	case class PoissonDist(lambda: Double) extends PoissonDistribution(lambda)	with DiscreteDist[PoissonDist]
	{ def getDist: PoissonDist = this }
	case class BinomialDist(numTrials: Int, p: Double) extends BinomialDistribution(numTrials, p)
		with DiscreteDist[BinomialDist] { def getDist: BinomialDist = this }
	case class GeometricDist(p: Double) extends GeometricDistribution(p)
		with DiscreteDist[GeometricDist] { def getDist: GeometricDist = this }

	case class GammaDist(shape: Double, scale: Double) extends GammaDistribution(shape, scale)
		with ContinuousDist[GammaDist] { def getDist: GammaDist = this }
	case class NormalDist(mu: Double, std: Double) extends NormalDistribution(mu, std)
		with ContinuousDist[NormalDist] { def getDist: NormalDist = this }
	case class UniformContinuousDist(a: Double, b: Double) extends UniformRealDistribution(a, b)
		with ContinuousDist[UniformContinuousDist] { def getDist: UniformContinuousDist = this }
	case class GumbelDist(mu: Double, beta: Double) extends GumbelDistribution(mu, beta)
		with ContinuousDist[GumbelDist] { def getDist: GumbelDist = this }


	object PoissonDist {
		// NOTE: used for the testUsage_CDFTD version
		implicit def poissonHasCDF = new CDF[Int, PoissonDist] {
			def cumulativeProbability(d: PoissonDist, x: Int): Double = d.cumulativeProbability(x)
		}


		//NOTE: this is the implementation that makes PoisAbsDist().cdf(_) work at last!!!
		// NOTE: used for the testUsage_CDFTATD version
		implicit def poissonDistHasCDF: CDF[Int, Dist[Int, PoissonDist]] = new CDF[Int, Dist[Int, PoissonDist]] {

			def cumulativeProbability(d: Dist[Int, PoissonDist], n: Int): Double = {
				d.getDist.cumulativeProbability(n)
			}
		}
	}
	import PoissonDist._

	object GammaDist {
		implicit def gammaDistHasCDF: CDF[Double, Dist[Double, GammaDist]] = new CDF[Double, Dist[Double,
			GammaDist]] {
			def cumulativeProbability(d: Dist[Double, GammaDist], x: Double): Double = {
				d.getDist.cumulativeProbability(x)
			}
		}
	}


	implicit class CDFSyntax[T: Numeric, D](current: Dist[T, D])(implicit ev: CDF[T, Dist[T, D]]){
		def cdf(x: T): Double = ev.cumulativeProbability(current, x)
			//ev.cumulativeProbability(ev.getDistFromCDFArea(current), x)
	}


	def testUsage_CDFTATD[T: Numeric, D](x: T, distAbs: Dist[T, D])(implicit ev: CDF[T, Dist[T, D]]): Double = {
		//distAbs.absdistCDF(x)
		distAbs.cdf(x)
	}


	// ===============================================================================================================
	def main(args: Array[String]) {

		Console.println(testUsage_CDFTATD[Int, PoissonDist](10, PoissonDist(3.4)))

		Console.println(testUsage_CDFTATD[Double, GammaDist](8.4, GammaDist(2, 2)))
	}

}