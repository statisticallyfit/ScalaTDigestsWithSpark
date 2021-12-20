package temp

import org.isarnproject.sketches.TDigest
import util.GeneralUtil
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._

import utilTest.TestData._
import utilTest.TestTools._

/**
 *
 */
object temp_conceptdrifttryout extends App {


	val gammaData1: Seq[Double] = GammaDist(20, 3).sample(SAMPLE_SIZE)
	val gammaData2: Seq[Double] = GammaDist(50, 3).sample(SAMPLE_SIZE)
	//List.fill[Double](SAMPLE_SIZE){ GammaDist(2,8).sample }

	val td1 = TDigest.sketch(gammaData1, maxDiscrete = MAX_DISCRETE)
	val td2 = TDigest.sketch(gammaData2, maxDiscrete = MAX_DISCRETE)

	val gammaMoveRight = TDigest.combine(td1, td2)

}
