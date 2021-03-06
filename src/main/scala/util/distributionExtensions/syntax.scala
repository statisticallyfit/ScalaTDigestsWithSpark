package util.distributionExtensions

import distributions._

/**
 *
 */
object syntax {

	// TODO try to make all the types work with syntax -- DiscrDist[D] as well as Dist[T, D] -- why not working?
	//  Need to put P[_,_] kind of thing?

	implicit class CDFSyntax[T: Numeric, D](distTD: Distr[T, D])(implicit ev: CDF[T, D]){

		def cdf(x: T): Double = ev.cumProb(distTD.getDist, x)

		def inverseCdf(p: Double): T = ev.invCumProb(distTD.getDist, p)
	}


	implicit class DiscreteCDFSyntax[D](distTD: DiscreteDist[D])(implicit ev: CDF[IntZ, DiscreteDist[D]]){

		def cdf(x: IntZ): Double = ev.cumProb(distTD, x)
		def inverseCdf(p: Double): IntZ = ev.invCumProb(distTD, p)
	}

	/*implicit class DiscreteCDFSyntax[T: Numeric, D, P[_] <: Dist[T, D]](distpd: P[D])(implicit ev: CDF[T, P[D]]){

		def cdf(x: T): Double = ev.cumulativeProbability(distpd, x)
		//ev.cumulativeProbability(ev.getDistFromCDFArea(current), x)
		def inverseCdf(p: Double): T = ev.inverseCumulativeProbability(distpd, p)
	}*/

	implicit class ContinuousCDFSyntax[D](distTD: ContinuousDist[D])(implicit ev: CDF[Real, ContinuousDist[D]]){

		def cdf(x: Real): Double = ev.cumProb(distTD, x)
		//ev.cumulativeProbability(ev.getDistFromCDFArea(current), x)
		def inverseCdf(p: Double): Real = ev.invCumProb(distTD, p)
	}

	implicit class SamplingSyntax[T: Numeric, D](distTD: Distr[T, D])(implicit ev: Sampling[T, D/*Distr[T, D]*/]){

		def sample(n: Int): Seq[T] = ev.sampleDist(distTD.getDist, n)
		def sample: T = ev.sampleDist(distTD.getDist, 1).head

	}

	implicit class ProbabilitySyntax[T: Numeric, D](distTD: Distr[T, D])(implicit ev: ProbabilityFunction[T, D]){
		// PMF if T is discrete (int) or PDF if T is continuous (double real)
		def probabilityFunction(x: T): Double = ev.prob(distTD.getDist, x)
	}
}
