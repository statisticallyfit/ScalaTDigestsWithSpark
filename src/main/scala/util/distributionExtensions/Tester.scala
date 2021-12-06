package util.distributionExtensions

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

/**
 *
 */
object Tester {

	def testUsage_CDFTATD[T: Numeric, D](x: T, distTD: Dist[T, D])(implicit ev1: CDF[T, Dist[T, D]],
													   ev2: Sampling[T, Dist[T, D]]): Unit = {
		//distAbs.absdistCDF(x)
		println(distTD.cdf(x))
		println(distTD.sample(3))
	}


	// ===============================================================================================================
	def main(args: Array[String]) {

		testUsage_CDFTATD[Int, PoissonDist](10, PoissonDist(3.4))

		testUsage_CDFTATD[Double, GammaDist](8.4, GammaDist(2, 2))

	}

}
