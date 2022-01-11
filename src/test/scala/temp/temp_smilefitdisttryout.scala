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
import smile.stat.distribution.GammaDistribution
import utilTest.TestTools.GeneralTools.repeatTail
/**
 *
 */
object temp_smilefitdisttryout extends App {


	val as = (30 to 180 by 10).toList
	val bs = ((2 to 10 by 1) ++ (10 to 60 by 10)).reverse
	val NUM_REPEAT = 6 // last 6 in alphas list (to weave in the small betas)

	val alphas = repeatTail(as)
	val betas = repeatTail(bs)
	// TODO how to check the important pairs at the end make it into the paired list?
	val asBs = alphas.zip(betas)

	val gammasMovingRight: Seq[GammaDist] = asBs.map { case (a, b) => GammaDist(a, b) }

	// NOTE: now here the NUM_MONOIDAL_ADDITIONS is comparable to shiftData.length
	val shiftData: Seq[Array[Double]] = gammasMovingRight.map(gdist => gdist.sample(SAMPLE_SIZE))

	// Creating the sketches and combining them:
	val shiftedSketch: Seq[TDigest] = shiftData
		.map((distSmp: Array[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here


	// see how convergence is at the last combination
	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE){shiftedSketch.last.samplePDF}


	val fit = GammaDistribution.fit(conceptDriftData)
	val distShifted = GammaDist(fit.k, fit.theta)
	val distFirst: GammaDist = gammasMovingRight.head
	val distLast: GammaDist = gammasMovingRight.last

	/*distShifted.getNumericalMean should beBetween(distFirst.getNumericalMean, distLast
		.getNumericalMean)

	// right-skew dist should be on the left of the combined dist (between the right and left-skewed dists)
	cdfSignTest(distFirst, distShifted) should beLessThan(0.0)
	// combined dist should be on the right of the left-tailed dist (between the right and left-skewed dists)
	cdfSignTest(distShifted, distLast) should beLessThan(0.0)*/

	// TODO shape seems confused with scale param here (from smile or from tdigest estimate?)
	println(s"alphas, betas: ${(distFirst.getAlpha, distFirst.getBeta)}, " +
		s"${(distShifted.getAlpha, distShifted.getBeta)}, " +
		s"${(distLast.getAlpha, distLast.getBeta)}")

	println(s"means: ${distLast.getNumericalMean} <? ${distShifted.getNumericalMean} <? " +
		s"${distFirst.getNumericalMean}")
	println(s"cdf sign test = ${cdfSignTest(distFirst, distShifted)}")
	println(s"cdf sign test = ${cdfSignTest(distShifted, distLast)}")

	val (a1, a2, a3, b1, b2, b3) = (distFirst.shape, distShifted.shape, distLast.shape, distFirst.scale, distShifted
		.scale, distLast.scale)
	println(s"modes = ${((a1-1)/b1, (a2-1)/b2, (a3-1)/b3)}")
	println()

	println(s"sign test of (2,2), (28,28): ${cdfSignTest(GammaDist(2,2), GammaDist(28,28))}")
	println(s"modes of (2,2), (28, 28): ${((2-1)/2,  (28-1)/28)}")
}
