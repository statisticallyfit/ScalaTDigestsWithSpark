package workspace_snippets


import org.isarnproject.sketches.TDigest

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._


import utilTest.TestData._
import utilTest.TestTools.StatTools._

import smile.stat.distribution.GammaDistribution



/**
 *
 */
object workspace_addSameGamma extends App {


	val (a1, b1) = (2.5, 10.0)
	val (a2, b2) = (35.0, 10.0) //(142.0, 15.0) //(4.5, 2.1)

	val distRightSkewStart = GammaDist(a1, b1)
	val distLeftSkewShift = GammaDist(a2, b2)

	val startingData: Seq[Double] = distRightSkewStart.sample(SAMPLE_SIZE)

	val shiftData: Seq[Seq[Double]] = Seq.fill[Seq[Double]](1 + NUM_MONOIDAL_ADDITIONS)(
		distLeftSkewShift.sample(SAMPLE_SIZE)
	)

	// Computing the T-Digest sketches, cumulatively, keeping track of the previous ones.
	//  Means; sum first 2, sum first 3, sum first 4, sum first 5, ... and keep track, all the way up to
	//  NUM_MONOIDAL_ADDITIONS.
	val startSketch: TDigest = TDigest.sketch(startingData/*, maxDiscrete = MAX_DISCRETE*/)

	// Adding the other-kind gamma to the single
	val shiftedSketch: Seq[TDigest] = shiftData
		.map((distSmp: Seq[Double]) => TDigest.sketch(distSmp/*, maxDiscrete = MAX_DISCRETE*/))
		.scanLeft(startSketch)((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here


	// see how convergence is at the last combination
	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE){shiftedSketch.last.samplePDF}


	val fit = GammaDistribution.fit(conceptDriftData)
	val distShifted = GammaDist(fit.k, fit.theta)


	println(s"\n\n\nmode of left (starting) = ${calcMode(distRightSkewStart)}")
	println(s"gamma left: ${distRightSkewStart}")
	println(s"mode of end = ${calcMode(distShifted)}")
	println(s"gamma end: ${distShifted}")
	println(s"cdf sign test: ${cdfSignTest(distRightSkewStart, distShifted)}")

	// NOTE: now adding the starting with the single-type accumulated one
	val pureRightSketch: Seq[TDigest] = shiftData
		.map((distSmp: Seq[Double]) => TDigest.sketch(distSmp/*, maxDiscrete = MAX_DISCRETE*/))
		.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

	val td = TDigest.combine(startSketch, pureRightSketch.last)
	val fitTD = GammaDistribution.fit(Array.fill[Double](SAMPLE_SIZE){td.samplePDF})
	val dist = GammaDist(fitTD.k, fitTD.theta)
	println(s"mode of end = ${calcMode(dist)}")
	println(s"gamma end: ${dist}")
}
