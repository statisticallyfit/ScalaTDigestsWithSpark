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

		implicit def poissonDistHasCDF: CDF[IntZ, PoissonDist] =
			new CDF[IntZ, PoissonDist] {

			def cumProb(d: PoissonDist, n: IntZ): Double = {
				d.cumulativeProbability(n.intValue())
				//d.getDist.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: PoissonDist, p: Double): IntZ = {
				//d.getDist.inverseCumulativeProbability(p)
				d.inverseCumulativeProbability(p)
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

		implicit def poissonDistCanBeSampled: Sampling[IntZ, PoissonDist] =
			new Sampling[IntZ, PoissonDist]{

			def sampleDist(d: PoissonDist, n: Int): Seq[IntZ] =
				d.sample(n.intValue()).map(BigInt(_))
		}
	}
	//import PoissonInstances._

	trait BinomialInstances {
		implicit def binomialHasProbability: ProbabilityFunction[IntZ, BinomialDist] =
			new ProbabilityFunction[IntZ, BinomialDist] {
				def prob(d: BinomialDist, x: IntZ): Double = d.probability(x.intValue())
			}

		implicit def binomialDistHCDF: CDF[IntZ, BinomialDist] =
			new CDF[IntZ, BinomialDist] {

			def cumProb(d: BinomialDist, n: IntZ): Double = {
				d.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: BinomialDist, p: Double): IntZ = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def BinomialDistCanBeSampled: Sampling[IntZ, BinomialDist] =
			new Sampling[IntZ, BinomialDist]{

			def sampleDist(d: BinomialDist, n: Int): Seq[IntZ] =
				d.sample(n.intValue()).map(BigInt(_))
		}
	}

	trait GeometricInstances {
		implicit def geometricHasProbability: ProbabilityFunction[IntZ, GeometricDist] =
			new ProbabilityFunction[IntZ, GeometricDist] {
				def prob(d: GeometricDist, x: IntZ): Double = d.probability(x.intValue())
			}

		implicit def geometricDistHCDF: CDF[IntZ, GeometricDist] =
			new CDF[IntZ, GeometricDist] {

			def cumProb(d: GeometricDist, n: IntZ): Double = {
				d.cumulativeProbability(n.intValue())
			}
			def invCumProb(d: GeometricDist, p: Double): IntZ = {
				d.inverseCumulativeProbability(p)
			}
		}

		implicit def geometricDistCanBeSampled: Sampling[IntZ, GeometricDist] =
			new Sampling[IntZ, GeometricDist]{

			def sampleDist(d: GeometricDist, n: Int): Seq[IntZ] =
				d.getDist.sample(n).map(BigInt(_))
		}

	}

	trait GammaInstances {
		implicit def gammaHasProbability: ProbabilityFunction[Real, GammaDist] =
			new ProbabilityFunction[Real, GammaDist] {
				def prob(d: GammaDist, x: Real): Double = d.density(x.doubleValue())
					// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def gammaDistHasCDF: CDF[Real, GammaDist] =
			new CDF[Real, GammaDist] {

			def cumProb(d: GammaDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: GammaDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def gammaDistCanBeSampled: Sampling[Real, GammaDist] =
			new Sampling[Real, GammaDist]{

			def sampleDist(d: GammaDist, n: Int): Seq[Real] =
				d.sample(n).map(BigDecimal(_))
		}

	}

	trait NormalInstances {
		implicit def normalHasProbability: ProbabilityFunction[Real, NormalDist] =
			new ProbabilityFunction[Real, NormalDist] {
				def prob(d: NormalDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def normalDistHasCDF: CDF[Real, NormalDist] = new CDF[Real, NormalDist] {

			def cumProb(d: NormalDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: NormalDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def normalDistCanBeSampled: Sampling[Real, NormalDist] =
			new Sampling[Real, NormalDist]{

			def sampleDist(d: NormalDist, n: Int): Seq[Real] =
				d.sample(n).map(BigDecimal(_))
		}
	}

	trait LogisticInstances {
		implicit def LogisticHasProbability: ProbabilityFunction[Real, LogisticDist] =
			new ProbabilityFunction[Real, LogisticDist] {
				def prob(d: LogisticDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def LogisticDistHasCDF: CDF[Real, LogisticDist] = new CDF[Real, LogisticDist] {

			def cumProb(d: LogisticDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: LogisticDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def LogisticDistCanBeSampled: Sampling[Real, LogisticDist] =
			new Sampling[Real, LogisticDist]{

				def sampleDist(d: LogisticDist, n: Int): Seq[Real] =
					d.sample(n).map(BigDecimal(_))
			}
	}

	trait ContinuousUniformInstances {
		implicit def uniformContinHasProbability: ProbabilityFunction[Real, ContinuousUniformDist] =
			new ProbabilityFunction[Real, ContinuousUniformDist] {
				def prob(d: ContinuousUniformDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def uniformContinuousDistHasCDF: CDF[Real, ContinuousUniformDist] = new
				CDF[Real, ContinuousUniformDist] {
			def cumProb(d: ContinuousUniformDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: ContinuousUniformDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def continuousUniformDistCanBeSampled: Sampling[Real, ContinuousUniformDist] =
			new Sampling[Real, ContinuousUniformDist]{

			def sampleDist(d: ContinuousUniformDist, n: Int): Seq[Real] =
				d.sample(n).map(BigDecimal(_))
		}
	}

	trait GumbelInstances {
		implicit def gumbelHasProbability: ProbabilityFunction[Real, GumbelDist] =
			new ProbabilityFunction[Real, GumbelDist] {
				def prob(d: GumbelDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def gumbelDistHasCDF: CDF[Real, GumbelDist] = new CDF[Real, GumbelDist] {
			def cumProb(d: GumbelDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: GumbelDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def gumbelDistCanBeSampled: Sampling[Real, GumbelDist] = new Sampling[Real,
			GumbelDist]{
			def sampleDist(d: GumbelDist, n: Int): Seq[Real] =
				d.sample(n).map(BigDecimal(_))
		}
	}

	trait ExponentialInstances {

		implicit def exponentialHasProbability: ProbabilityFunction[Real, ExponentialDist] =
			new ProbabilityFunction[Real, ExponentialDist] {
				def prob(d: ExponentialDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def exponentialDistHasCDF: CDF[Real, ExponentialDist] = new CDF[Real, ExponentialDist] {
			def cumProb(d: ExponentialDist, x: Real): Double = {
				d.getDist.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: ExponentialDist, p: Double): Real = {
				d.getDist.inverseCumulativeProbability(p)
			}
		}
		implicit def exponentialDistCanBeSampled: Sampling[Real, ExponentialDist] = new Sampling[Real,
			ExponentialDist]{
			def sampleDist(d: ExponentialDist, n: Int): Seq[Real] =
				d.getDist.sample(n).map(BigDecimal(_))
		}
	}

	trait BetaInstances {
		implicit def betaHasProbability: ProbabilityFunction[Real, BetaDist] =
			new ProbabilityFunction[Real, BetaDist] {
				def prob(d: BetaDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def betaDistHasCDF: CDF[Real, BetaDist] = new CDF[Real, BetaDist] {

			def cumProb(d: BetaDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: BetaDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def betaDistCanBeSampled: Sampling[Real, BetaDist] =
			new Sampling[Real, BetaDist]{

				def sampleDist(d: BetaDist, n: Int): Seq[Real] =
					d.sample(n).map(BigDecimal(_))
			}
	}

	trait WeibullInstances {
		implicit def weibullHasProbability: ProbabilityFunction[Real, WeibullDist] =
			new ProbabilityFunction[Real, WeibullDist] {
				def prob(d: WeibullDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def weibullDistHasCDF: CDF[Real, WeibullDist] = new CDF[Real, WeibullDist] {

			def cumProb(d: WeibullDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: WeibullDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def weibullDistCanBeSampled: Sampling[Real, WeibullDist] =
			new Sampling[Real, WeibullDist]{

				def sampleDist(d: WeibullDist, n: Int): Seq[Real] =
					d.sample(n).map(BigDecimal(_))
			}
	}

	trait ChiSquaredInstances {
		implicit def chiSquaredHasProbability: ProbabilityFunction[Real, ChiSquareDist] =
			new ProbabilityFunction[Real, ChiSquareDist] {
				def prob(d: ChiSquareDist, x: Real): Double = d.density(x.doubleValue())
				// TODO prob or density here?? d.probability(x.doubleValue())
			}

		implicit def chiSquaredDistHasCDF: CDF[Real, ChiSquareDist] = new CDF[Real, ChiSquareDist] {

			def cumProb(d: ChiSquareDist, x: Real): Double = {
				d.cumulativeProbability(x.doubleValue())
			}
			def invCumProb(d: ChiSquareDist, p: Double): Real = {
				d.inverseCumulativeProbability(p)
			}
		}
		implicit def chiSquaredDistCanBeSampled: Sampling[Real, ChiSquareDist] =
			new Sampling[Real, ChiSquareDist]{

				def sampleDist(d: ChiSquareDist, n: Int): Seq[Real] =
					d.sample(n).map(BigDecimal(_))
			}
	}


	trait DiscreteInstances extends PoissonInstances
		with GeometricInstances
		with BinomialInstances


	trait ContinuousInstances extends NormalInstances
		with GammaInstances
		with ContinuousUniformInstances
		with GumbelInstances
		with LogisticInstances
		with BetaInstances
		with ExponentialInstances
		with WeibullInstances
		with ChiSquaredInstances

	object AllInstances extends DiscreteInstances with ContinuousInstances
}
