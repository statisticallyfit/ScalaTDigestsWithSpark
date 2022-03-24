package util.distributionExtensions

import distributions._
/**
 *
 */
object instances {

	trait PoissonInstances {

		implicit def poissonHasProbability: ProbabilityFunction[IntZ, PoissonDist] =
			new ProbabilityFunction[IntZ, PoissonDist] {
				def prob(d: PoissonDist, x: IntZ): Double = d.probability(x.intValue())
			}

		implicit def poissonDistHasCDF: CDF[IntZ, Distr[IntZ, PoissonDist]] =
			new CDF[IntZ, Distr[IntZ, PoissonDist]] {

			def cumProb(d: Distr[IntZ, PoissonDist], n: IntZ): Double = {
				d.getDist.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: Distr[IntZ, PoissonDist], p: Double): IntZ = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def poissonDiscreteDistHasCDF: CDF[IntZ, DiscreteDist[PoissonDist]] =
			new CDF[IntZ, DiscreteDist[PoissonDist]] {

			def cumProb(d: DiscreteDist[PoissonDist], n: IntZ): Double = {
				d.getDist.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: DiscreteDist[PoissonDist], p: Double): IntZ = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		// TODO: try to avoid this repetition - try to declare CDF[T, D, P[_,_]]?
		// MOTIVation: the shortrange-longrange functions in kolmogorovtryout
		/*implicit def poissonDiscreteDistHasCDF[P[_] <: Dist[Z, PoissonDist]]: CDF[Z, P[PoissonDist]] = new
				CDF[Z, P[PoissonDist]] {

			def cumulativeProbability(d: P[PoissonDist], n: Z): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: P[PoissonDist], p: Double): Z = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}*/
		/*implicit def poissonDistHasCDF: CDF[Z, PoissonDist] = new CDF[Z, PoissonDist] {

			def cumulativeProbability(d: Dist[Z, PoissonDist], n: Z): Double = {
				d.getDist.cumulativeProbability(n)
			}
			def inverseCumulativeProbability(d: Dist[Z, PoissonDist], p: Double): Z = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}*/

		implicit def poissonDistCanBeSampled: Sampling[IntZ, Distr[IntZ, PoissonDist]] =
			new Sampling[IntZ, Distr[IntZ, PoissonDist]]{

			def sampleDist(d: Distr[IntZ, PoissonDist], n: Int): Seq[IntZ] =
				d.getDist.sample(n.intValue()).map(BigInt(_))
		}
	}
	//import PoissonInstances._

	trait BinomialInstances {
		implicit def binomialHasProbability: ProbabilityFunction[IntZ, BinomialDist] =
			new ProbabilityFunction[IntZ, BinomialDist] {
				def prob(d: BinomialDist, x: IntZ): Double = d.probability(x.intValue())
			}

		implicit def binomialDistHCDF: CDF[IntZ, Distr[IntZ, BinomialDist]] =
			new CDF[IntZ, Distr[IntZ, BinomialDist]] {

			def cumProb(d: Distr[IntZ, BinomialDist], n: IntZ): Double = {
				d.getDist.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: Distr[IntZ, BinomialDist], p: Double): IntZ = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def BinomialDistCanBeSampled: Sampling[IntZ, Distr[IntZ, BinomialDist]] =
			new Sampling[IntZ, Distr[IntZ, BinomialDist]]{

			def sampleDist(d: Distr[IntZ, BinomialDist], n: Int): Seq[IntZ] =
				d.getDist.sample(n.intValue()).map(BigInt(_))
		}
	}

	trait GeometricInstances {
		implicit def geometricHasProbability: ProbabilityFunction[IntZ, GeometricDist] =
			new ProbabilityFunction[IntZ, GeometricDist] {
				def prob(d: GeometricDist, x: IntZ): Double = d.probability(x.intValue())
			}

		implicit def geometricDistHCDF: CDF[IntZ, Distr[IntZ, GeometricDist]] =
			new CDF[IntZ, Distr[IntZ, GeometricDist]] {

			def cumProb(d: Distr[IntZ, GeometricDist], n: IntZ): Double = {
				d.getDist.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: Distr[IntZ, GeometricDist], p: Double): IntZ = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}

		implicit def geometricDistCanBeSampled: Sampling[IntZ, Distr[IntZ, GeometricDist]] =
			new Sampling[IntZ, Distr[IntZ, GeometricDist]]{

			def sampleDist(d: Distr[IntZ, GeometricDist], n: Int): Seq[IntZ] =
				d.getDist.sample(n).map(BigInt(_))
		}

	}

	trait GammaInstances {
		implicit def gammaHasProbability: ProbabilityFunction[Real, GammaDist] =
			new ProbabilityFunction[Real, GammaDist] {
				def prob(d: GammaDist, x: Real): Double = d.density(x.doubleValue())
					// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def gammaDistHasCDF: CDF[Real, Distr[Real, GammaDist]] =
			new CDF[Real, Distr[Real, GammaDist]] {

			def cumProb(d: Distr[Real, GammaDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, GammaDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def gammaDistCanBeSampled: Sampling[Real, Distr[Real, GammaDist]] =
			new Sampling[Real, Distr[Real, GammaDist]]{

			def sampleDist(d: Distr[Real, GammaDist], n: Int): Seq[Real] =
				d.getDist.sample(n).map(BigDecimal(_))
		}

	}

	trait NormalInstances {
		implicit def normalHasProbability: ProbabilityFunction[Real, NormalDist] =
			new ProbabilityFunction[Real, NormalDist] {
				def prob(d: NormalDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def normalDistHasCDF: CDF[Real, Distr[Real, NormalDist]] = new CDF[Real, Distr[Real, NormalDist]] {

			def cumProb(d: Distr[Real, NormalDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, NormalDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def normalDistCanBeSampled: Sampling[Real, Distr[Real, NormalDist]] =
			new Sampling[Real, Distr[Real, NormalDist]]{

			def sampleDist(d: Distr[Real, NormalDist], n: Int): Seq[Real] =
				d.getDist.sample(n).map(BigDecimal(_))
		}
	}

	trait ContinuousUniformInstances {
		implicit def uniformContinHasProbability: ProbabilityFunction[Real, ContinuousUniformDist] =
			new ProbabilityFunction[Real, ContinuousUniformDist] {
				def prob(d: ContinuousUniformDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def uniformContinuousDistHasCDF: CDF[Real, Distr[Real, ContinuousUniformDist]] = new
				CDF[Real, Distr[Real, ContinuousUniformDist]] {
			def cumProb(d: Distr[Real, ContinuousUniformDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, ContinuousUniformDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def continuousUniformDistCanBeSampled: Sampling[Real, Distr[Real, ContinuousUniformDist]] =
			new Sampling[Real, Distr[Real, ContinuousUniformDist]]{

			def sampleDist(d: Distr[Real, ContinuousUniformDist], n: Int): Seq[Real] =
				d.getDist.sample(n).map(BigDecimal(_))
		}
	}

	trait GumbelInstances {
		implicit def gumbelHasProbability: ProbabilityFunction[Real, GumbelDist] =
			new ProbabilityFunction[Real, GumbelDist] {
				def prob(d: GumbelDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def gumbelDistHasCDF: CDF[Real, Distr[Real, GumbelDist]] = new CDF[Real, Distr[Real,
			GumbelDist]] {
			def cumProb(d: Distr[Real, GumbelDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, GumbelDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def gumbelDistCanBeSampled: Sampling[Real, Distr[Real, GumbelDist]] = new Sampling[Real,
			Distr[Real, GumbelDist]]{
			def sampleDist(d: Distr[Real, GumbelDist], n: Int): Seq[Real] =
				d.getDist.sample(n).map(BigDecimal(_))
		}
	}

	trait ExponentialInstances {

		implicit def exponentialHasProbability: ProbabilityFunction[Real, ExponentialDist] =
			new ProbabilityFunction[Real, ExponentialDist] {
				def prob(d: ExponentialDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def exponentialDistHasCDF: CDF[Real, Distr[Real, ExponentialDist]] = new CDF[Real, Distr[Real,
			ExponentialDist]] {
			def cumProb(d: Distr[Real, ExponentialDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, ExponentialDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def exponentialDistCanBeSampled: Sampling[Real, Distr[Real, ExponentialDist]] = new Sampling[Real,
			Distr[Real, ExponentialDist]]{
			def sampleDist(d: Distr[Real, ExponentialDist], n: Int): Seq[Real] =
				d.getDist.sample(n).map(BigDecimal(_))
		}
	}

	trait BetaInstances {
		implicit def betaHasProbability: ProbabilityFunction[Real, BetaDist] =
			new ProbabilityFunction[Real, BetaDist] {
				def prob(d: BetaDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def betaDistHasCDF: CDF[Real, Distr[Real, BetaDist]] = new CDF[Real, Distr[Real, BetaDist]] {

			def cumProb(d: Distr[Real, BetaDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, BetaDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def betaDistCanBeSampled: Sampling[Real, Distr[Real, BetaDist]] =
			new Sampling[Real, Distr[Real, BetaDist]]{

				def sampleDist(d: Distr[Real, BetaDist], n: Int): Seq[Real] =
					d.getDist.sample(n).map(BigDecimal(_))
			}
	}

	trait WeibullInstances {
		implicit def weibullHasProbability: ProbabilityFunction[Real, WeibullDist] =
			new ProbabilityFunction[Real, WeibullDist] {
				def prob(d: WeibullDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def weibullDistHasCDF: CDF[Real, Distr[Real, WeibullDist]] = new CDF[Real, Distr[Real, WeibullDist]] {

			def cumProb(d: Distr[Real, WeibullDist], x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: Distr[Real, WeibullDist], p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def weibullDistCanBeSampled: Sampling[Real, Distr[Real, WeibullDist]] =
			new Sampling[Real, Distr[Real, WeibullDist]]{

				def sampleDist(d: Distr[Real, WeibullDist], n: Int): Seq[Real] =
					d.getDist.sample(n).map(BigDecimal(_))
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
