package util.distributionExtensions


import util.distributionExtensions.distributions._

/**
 *
 */
object syntax {

	// TODO try to make all the types work with syntax -- DiscrDist[D] as well as Dist[T, D] -- why not working?
	//  Need to put P[_,_] kind of thing?

	implicit class CDFSyntax[T: Numeric, D](distTD: Dist[T, D])(implicit ev: CDF[T, Dist[T, D]]){

		def cdf(x: T): Double = ev.cumulativeProbability(distTD, x)

		def inverseCdf(p: Double): T = ev.inverseCumulativeProbability(distTD, p)
	}


	implicit class DiscreteCDFSyntax[D](distpd: DiscreteDist[D])(implicit ev: CDF[Int, DiscreteDist[D]]){

		def cdf(x: Int): Double = ev.cumulativeProbability(distpd, x)
		def inverseCdf(p: Double): Int = ev.inverseCumulativeProbability(distpd, p)
	}

	/*implicit class DiscreteCDFSyntax[T: Numeric, D, P[_] <: Dist[T, D]](distpd: P[D])(implicit ev: CDF[T, P[D]]){

		def cdf(x: T): Double = ev.cumulativeProbability(distpd, x)
		//ev.cumulativeProbability(ev.getDistFromCDFArea(current), x)
		def inverseCdf(p: Double): T = ev.inverseCumulativeProbability(distpd, p)
	}*/

	implicit class ContinuousCDFSyntax[D](distTD: ContinuousDist[D])(implicit ev: CDF[Double, ContinuousDist[D]]){

		def cdf(x: Double): Double = ev.cumulativeProbability(distTD, x)
		//ev.cumulativeProbability(ev.getDistFromCDFArea(current), x)
		def inverseCdf(p: Double): Double = ev.inverseCumulativeProbability(distTD, p)
	}

	implicit class SamplingSyntax[T: Numeric, D](distTD: Dist[T, D])(implicit ev: Sampling[T, Dist[T, D]]){

		def sample(n: Int): Seq[T] = ev.sampleDist(distTD, n)
		def sample: T = ev.sampleDist(distTD, 1).head

	}

}
