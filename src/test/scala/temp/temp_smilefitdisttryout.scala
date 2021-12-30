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
object temp_smilefitdisttryout extends App {


	//val (a1, a2) = (5.0, 80.0) // the shift, defined (from 20 -> 50, dist moves right)
	val (a1, b1) = (2.5, 10.0)
	val (a2, b2) = (4.5, 2.1)
	//val b = 38.0

	val startingData: Seq[Double] = GammaDist(a2, b2).sample(SAMPLE_SIZE)
	//val otherData: Seq[Double] = GammaDist(a2, b2).sample(SAMPLE_SIZE)

	val otherData: Seq[Seq[Double]] = Seq.fill[Seq[Double]](1 + NUM_MONOIDAL_ADDITIONS_LARGE)(
		GammaDist(a1, b1).sample(SAMPLE_SIZE)
	)


	// Computing the T-Digest sketches, cumulatively, keeping track of the previous ones.
	//  Means; sum first 2, sum first 3, sum first 4, sum first 5, ... and keep track, all the way up to
	//  NUM_MONOIDAL_ADDITIONS.
	val startSketch: TDigest = TDigest.sketch(startingData, maxDiscrete = MAX_DISCRETE)

	val shifted: Seq[TDigest] = otherData
		.map((distSmp: Seq[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(startSketch)((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

	//List.fill[Double](SAMPLE_SIZE){ GammaDist(2,8).sample }

	val shifted0: TDigest = shifted.last // see how convergence is at the last combination

	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE){shifted0.samplePDF}

	import smile.stat.distribution.GammaDistribution
	val est = GammaDistribution.fit(conceptDriftData)
	val (alphaShape, betaScale) = (est.k, est.theta)

	// TODO shape seems confused with scale param here (from smile or from tdigest estimate?)
	println(alphaShape, betaScale)
}
