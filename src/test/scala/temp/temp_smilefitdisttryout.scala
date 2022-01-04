package temp

import org.isarnproject.sketches.TDigest
import util.GeneralUtil
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._

import utilTest.TestData._
import utilTest.TestTools.StatTools._
import utilTest.TestTools.SpecsTools._
/**
 *
 */
object temp_smilefitdisttryout extends App {


	//val (a1, a2) = (5.0, 80.0) // the shift, defined (from 20 -> 50, dist moves right)
	val (a1, b1) = (2.5, 10.0)
	val (a2, b2) = (4.5, 2.1)

	val distRightSkewStart = GammaDist(a1, b1)
	val distLeftSkewShift = GammaDist(a2, b2)

	val startingData: Seq[Double] = distRightSkewStart.sample(SAMPLE_SIZE)

	val shiftData: Seq[Seq[Double]] = Seq.fill[Seq[Double]](1 + NUM_MONOIDAL_ADDITIONS)(
		distLeftSkewShift.sample(SAMPLE_SIZE)
	)


	// Computing the T-Digest sketches, cumulatively, keeping track of the previous ones.
	//  Means; sum first 2, sum first 3, sum first 4, sum first 5, ... and keep track, all the way up to
	//  NUM_MONOIDAL_ADDITIONS.
	val startSketch: TDigest = TDigest.sketch(startingData, maxDiscrete = MAX_DISCRETE)

	// Adding the other-kind gamma to the single
	val shiftedSketch: Seq[TDigest] = shiftData
		.map((distSmp: Seq[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(startSketch)((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

	//List.fill[Double](SAMPLE_SIZE){ GammaDist(2,8).sample }

	// see how convergence is at the last combination
	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE){shiftedSketch.last.samplePDF}

	import smile.stat.distribution.GammaDistribution
	val distShifted = GammaDistribution.fit(conceptDriftData)
	val (alphaShape, betaScale) = (distShifted.k, distShifted.theta)

	distShifted.mean() should beBetween(distRightSkewStart.getNumericalMean, distLeftSkewShift.getNumericalMean)

	// TODO shape seems confused with scale param here (from smile or from tdigest estimate?)
	println(alphaShape, betaScale)
	println(s"means: ${distLeftSkewShift.getNumericalMean} <? ${distShifted.mean()} <? " +
		s"${distRightSkewStart.getNumericalMean}")
	println(s"cdf sign test = ${cdfSignTest(distRightSkewStart, distLeftSkewShift)}")
	println(s"cdf sign test = ${cdfSignTest(distLeftSkewShift, distRightSkewStart)}")
}
