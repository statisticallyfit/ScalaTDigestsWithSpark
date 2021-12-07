package util.distributionExtensions

import util.distributionExtensions.distributions._
/**
 *
 */
object instances {

	trait PoissonInstances {

		implicit def poissonDistHasCDF: CDF[Int, Dist[Int, PoissonDist]] = new CDF[Int, Dist[Int, PoissonDist]] {

			def cumulativeProbability(d: Dist[Int, PoissonDist], n: Int): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: Dist[Int, PoissonDist], p: Double): Int = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def poissonDiscreteDistHasCDF: CDF[Int, DiscreteDist[PoissonDist]] = new CDF[Int,
			DiscreteDist[PoissonDist]] {

			def cumulativeProbability(d: DiscreteDist[PoissonDist], n: Int): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: DiscreteDist[PoissonDist], p: Double): Int = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		// TODO: try to avoid this repetition - try to declare CDF[T, D, P[_,_]]?
		// MOTIVation: the shortrange-longrange functions in kolmogorovtryout
		/*implicit def poissonDiscreteDistHasCDF[P[_] <: Dist[Int, PoissonDist]]: CDF[Int, P[PoissonDist]] = new
				CDF[Int, P[PoissonDist]] {

			def cumulativeProbability(d: P[PoissonDist], n: Int): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: P[PoissonDist], p: Double): Int = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}*/
		/*implicit def poissonDistHasCDF: CDF[Int, PoissonDist] = new CDF[Int, PoissonDist] {

			def cumulativeProbability(d: Dist[Int, PoissonDist], n: Int): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: Dist[Int, PoissonDist], p: Double): Int = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}*/

		implicit def poissonDistCanBeSampled: Sampling[Int, Dist[Int, PoissonDist]] = new Sampling[Int, Dist[Int,
			PoissonDist]]{
			def sampleDist(d: Dist[Int, PoissonDist], n: Int): Seq[Int] = d.getDist.sample(n)
		}
	}
	//import PoissonInstances._

	trait BinomialInstances {
		implicit def binomialDistHCDF: CDF[Int, Dist[Int, BinomialDist]] = new CDF[Int, Dist[Int,
			BinomialDist]] {
			def cumulativeProbability(d: Dist[Int, BinomialDist], n: Int): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: Dist[Int, BinomialDist], p: Double): Int = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def BinomialDistCanBeSampled: Sampling[Int, Dist[Int, BinomialDist]] = new Sampling[Int, Dist[Int,
			BinomialDist]]{
			def sampleDist(d: Dist[Int, BinomialDist], n: Int): Seq[Int] = d.getDist.sample(n)
		}
	}

	trait GeometricInstances {
		implicit def geometricDistHCDF: CDF[Int, Dist[Int, GeometricDist]] = new CDF[Int, Dist[Int,
			GeometricDist]] {
			def cumulativeProbability(d: Dist[Int, GeometricDist], x: Int): Double = {
				d.getDist.cumulativeProbability(x)
			}
			def inverseCumulativeProbability(d: Dist[Int, GeometricDist], p: Double): Int = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}

		implicit def geometricDistCanBeSampled: Sampling[Int, Dist[Int, GeometricDist]] = new Sampling[Int, Dist[Int,
			GeometricDist]]{
			def sampleDist(d: Dist[Int, GeometricDist], n: Int): Seq[Int] = d.getDist.sample(n)
		}

	}

	trait GammaInstances {
		implicit def gammaDistHasCDF: CDF[Double, Dist[Double, GammaDist]] = new CDF[Double, Dist[Double,
			GammaDist]] {
			def cumulativeProbability(d: Dist[Double, GammaDist], x: Double): Double = {
				d.getDist.cumulativeProbability(x)
			}
			def inverseCumulativeProbability(d: Dist[Double, GammaDist], p: Double): Double = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def gammaDistCanBeSampled: Sampling[Double, Dist[Double, GammaDist]] = new Sampling[Double, Dist[Double,
			GammaDist]]{
			def sampleDist(d: Dist[Double, GammaDist], n: Int): Seq[Double] = d.getDist.sample(n)
		}

	}

	trait NormalInstances {
		implicit def normalDistHasCDF: CDF[Double, Dist[Double, NormalDist]] = new CDF[Double, Dist[Double,
			NormalDist]] {
			def cumulativeProbability(d: Dist[Double, NormalDist], x: Double): Double = {
				d.getDist.cumulativeProbability(x)
			}
			def inverseCumulativeProbability(d: Dist[Double, NormalDist], p: Double): Double = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def normalDistCanBeSampled: Sampling[Double, Dist[Double, NormalDist]] = new Sampling[Double, Dist[Double,
			NormalDist]]{
			def sampleDist(d: Dist[Double, NormalDist], n: Int): Seq[Double] = d.getDist.sample(n)
		}
	}

	trait ContinuousUniformInstances {
		implicit def uniformContinuousDistHasCDF: CDF[Double, Dist[Double, ContinuousUniformDist]] = new
				CDF[Double, Dist[Double, ContinuousUniformDist]] {
			def cumulativeProbability(d: Dist[Double, ContinuousUniformDist], x: Double): Double = {
				d.getDist.cumulativeProbability(x)
			}
			def inverseCumulativeProbability(d: Dist[Double, ContinuousUniformDist], p: Double): Double = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def continuousUniformDistCanBeSampled: Sampling[Double, Dist[Double, ContinuousUniformDist]] = new Sampling[Double, Dist[Double, ContinuousUniformDist]]{
			def sampleDist(d: Dist[Double, ContinuousUniformDist], n: Int): Seq[Double] = d.getDist.sample(n)
		}
	}

	trait GumbelInstances {
		implicit def gumbelDistHasCDF: CDF[Double, Dist[Double, GumbelDist]] = new CDF[Double, Dist[Double,
			GumbelDist]] {
			def cumulativeProbability(d: Dist[Double, GumbelDist], x: Double): Double = {
				d.getDist.cumulativeProbability(x)
			}
			def inverseCumulativeProbability(d: Dist[Double, GumbelDist], p: Double): Double = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def gumbelDistCanBeSampled: Sampling[Double, Dist[Double, GumbelDist]] = new Sampling[Double,
			Dist[Double, GumbelDist]]{
			def sampleDist(d: Dist[Double, GumbelDist], n: Int): Seq[Double] = d.getDist.sample(n)
		}
	}

	trait ExponentialInstances {
		implicit def exponentialDistHasCDF: CDF[Double, Dist[Double, ExponentialDist]] = new CDF[Double, Dist[Double,
			ExponentialDist]] {
			def cumulativeProbability(d: Dist[Double, ExponentialDist], x: Double): Double = {
				d.getDist.cumulativeProbability(x)
			}
			def inverseCumulativeProbability(d: Dist[Double, ExponentialDist], p: Double): Double = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def exponentialDistCanBeSampled: Sampling[Double, Dist[Double, ExponentialDist]] = new Sampling[Double,
			Dist[Double, ExponentialDist]]{
			def sampleDist(d: Dist[Double, ExponentialDist], n: Int): Seq[Double] = d.getDist.sample(n)
		}
	}


	trait DiscreteCDFInstances extends PoissonInstances
		with GeometricInstances
		with BinomialInstances


	trait ContinuousCDFInstances extends NormalInstances
		with GammaInstances
		with ContinuousUniformInstances
		with GumbelInstances

	object AllInstances extends DiscreteCDFInstances with ContinuousCDFInstances

}
