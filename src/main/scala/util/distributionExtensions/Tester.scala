package util.distributionExtensions

import distributions._
import instances.AllInstances._
import syntax._

/**
 *
 */
object Tester {

	def testUsage_CDFTATD[T: Numeric, D](x: T, distTD: Distr[T, D])(implicit ev1: CDF[T, D],
													    ev2: Sampling[T, D]): Unit = {
		//distAbs.absdistCDF(x)
		println(distTD.toString)
		println(distTD.cdf(x))
		println(distTD.sample(3))
	}


	// ===============================================================================================================
	def main(args: Array[String]) {

		testUsage_CDFTATD[IntZ, PoissonDist](10, PoissonDist(3.4))

		testUsage_CDFTATD[Real, GammaDist](8.4, GammaDist(2, 2))

	}

}
