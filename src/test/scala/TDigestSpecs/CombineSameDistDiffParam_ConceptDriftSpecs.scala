package TDigestSpecs


import org.isarnproject.sketches.TDigest
import org.specs2.matcher.{Expectable, Matcher, MatchersImplicits, ShouldMatchers}
import org.specs2.mutable._
import util.GeneralUtil
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._

import utilTest.TestData._
import utilTest.TestTools.StatTools._
import utilTest.TestTools.SpecsTools._
import utilTest.TestTools.GeneralTools._

import smile.stat.distribution.GammaDistribution

/**
 * Idea of concept drift = https://github.com/xxxnell/flip#the-case-of-concept-drift
 */
class CombineSameDistDiffParam_ConceptDriftSpecs extends Specification {

	// MAJOR TODO: use the math theorems (poisson n -> infinity yields normal dist) to demonstrate it using the t-sketch
	//  method below here (under this distribution shift paradigm of this specs2 test)

	" (Continuous: Gamma) TDigest can combine sketches of distributions with different parameters" should {

		"---> combine once, by combining two same distributions with different parameters, and seeing how the " +
			"result lands in the middle" in {
			val (a1, b1) = (2.5, 10.0)
			val (a2, b2) = (4.5, 2.1)

			val distRightSkewStart = GammaDist(a1, b1)
			val distLeftSkewShift = GammaDist(a2, b2)

			val startingData: Seq[Double] = distRightSkewStart.sample(SAMPLE_SIZE_TEST)

			val shiftDataOnce: Array[Double] = distLeftSkewShift.sample(SAMPLE_SIZE_TEST)

			val td1 = TDigest.sketch(startingData)
			val td2 = TDigest.sketch(shiftDataOnce)

			val combineOnce = TDigest.combine(td1, td2)
			val combineOnceData = Array.fill[Double](SAMPLE_SIZE_TEST){combineOnce.samplePDF}

			val fit: GammaDistribution = GammaDistribution.fit(combineOnceData)
			val distCombineOnce: GammaDist = GammaDist(fit.k, fit.theta)


			// Mean of the shifted dist should be between the other two dists from which it was combined
			distCombineOnce.getNumericalMean should beBetween(distLeftSkewShift.getNumericalMean,
				distRightSkewStart.getNumericalMean)

			// right-skew dist should be on the left of the combined dist (between the right and left-skewed dists)
			cdfSignTest(distRightSkewStart, distCombineOnce) should beLessThan(0.0)
			// combined dist should be on the right of the left-tailed dist (between the right and left-skewed dists)
			cdfSignTest(distCombineOnce, distLeftSkewShift) should beLessThan(0.0)
		}

		// combine multiple sketches (case 1) where adding same gamma to original one. (different params)

		"---> combine multiple sketches (case 1), by adding many of the same distribution to one different " +
			"distribution, to see how the result gets shifted by the multiple combinations" in {
			val (a1, b1) = (2.5, 10.0)
			val (a2, b2) = (4.5, 2.1)

			val distRightSkewStart = GammaDist(a1, b1)
			val distLeftSkewShift = GammaDist(a2, b2)

			val startingData: Seq[Double] = distRightSkewStart.sample(SAMPLE_SIZE_TEST)

			val shiftData: Seq[Seq[Double]] = Seq.fill[Seq[Double]](1 + NUM_MONOIDAL_ADDITIONS)(
				distLeftSkewShift.sample(SAMPLE_SIZE_TEST)
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


			// see how convergence is at the last combination
			val conceptDriftData = Array.fill[Double](SAMPLE_SIZE_TEST){shiftedSketch.last.samplePDF}


			val fit = GammaDistribution.fit(conceptDriftData)
			val distShifted = GammaDist(fit.k, fit.theta)

			distShifted.getNumericalMean should beBetween(distLeftSkewShift.getNumericalMean, distRightSkewStart.getNumericalMean)

			// right-skew dist should be on the left of the combined dist (between the right and left-skewed dists)
			cdfSignTest(distRightSkewStart, distShifted) should beLessThan(0.0)
			// combined dist should be on the right of the left-tailed dist (between the right and left-skewed dists)
			cdfSignTest(distShifted, distLeftSkewShift) should beLessThan(0.0)

		}


		"---> combine multiple sketches (case 2) where adding a bunch of moving gammas (gammas that change " +
			"parameters) to the original one to see how the result gets shifted by the multiple combinations" in {

			// NOTE RULE GAMMA:
			// 1) increase ALPHA (shape), decrease BETA (scale) ---> distribution gets flatter + moves right
			// 2) decrease ALPHA, increase BETA ---> distribution gets narrower, taller + moves left

			/*val as = (30 to 180 by 10).toList
			val bs = ((2 to 10 by 1) ++ (10 to 60 by 10)).reverse
			val alphas = repeatTail(as)
			val betas = repeatTail(bs)
			// TODO how to check the important pairs at the end make it into the paired list?
			val asBs = alphas.zip(betas)*/

			val as = (5 to 150).map(_.toDouble).toList
			val bs = (5 to 150).map(_.toDouble).toList
			val asBs = for {
				x <- as
				y <- bs
			} yield (x, y)
			val gammas: Seq[GammaDist] = asBs.map { case (a, b) => GammaDist(a, b) }

			// Calculate the modes to order the gammas by their modes
			val modes = gammas.map(g => calcMode(g)).filter(_ > 1) //since the distributions with 0 are
			// too skewed to the left of the x-axis
			// Sort the dists by the modes, increasingly, so the dists are moving right
			val gammasMovingRight: Seq[GammaDist] = modes.zip(gammas)
				.sortBy{ case (mode, gammaDist) => mode }
				.unzip._2 // get just the gamma dists



			// NOTE: now here the NUM_MONOIDAL_ADDITIONS is replaced by shiftData.length
			val shiftData: Seq[Array[Double]] = gammasMovingRight.map(gdist => gdist.sample(SAMPLE_SIZE_TEST))

			// Creating the sketches and combining them:
			val shiftedSketch: Seq[TDigest] = shiftData
				.map((distSmp: Array[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
				.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
				.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here


			// see how convergence is at the last combination
			val conceptDriftData = Array.fill[Double](SAMPLE_SIZE_TEST){shiftedSketch.last.samplePDF}


			val fit = GammaDistribution.fit(conceptDriftData)
			val distShifted = GammaDist(fit.k, fit.theta)
			val distFirst: GammaDist = gammas.head
			val distLast: GammaDist = gammas.last

			distShifted.getNumericalMean should beBetween(distFirst.getNumericalMean, distLast
				.getNumericalMean)

			calcMode(distShifted) should beBetween(calcMode(distFirst), calcMode(distLast))

			// right-skew dist should be on the left of the combined dist (between the right and left-skewed dists)
			cdfSignTest(distFirst, distShifted) should beLessThan(0.0)
			// combined dist should be on the right of the left-tailed dist (between the right and left-skewed dists)
			cdfSignTest(distShifted, distLast) should beLessThan(0.0)
		}


	}



	/*" (Discrete: Poisson) TDigest can combine sketches to yield the same distribution" should {

		"---> combine once" in {
			//TEST_ID = (1, 'a')

			// NOTE: the lower the mean parameter, the more right-skewed. As lambda -> infinity, poisson -> normal
			//  (rule? check)
			val poissonData: Seq[Int] = PoissonDist(2).sample(SAMPLE_SIZE)
			//List.fill[Double](SAMPLE_SIZE){ GammaDist(2,8).sample }

			val td1 = TDigest.sketch(poissonData, maxDiscrete = MAX_DISCRETE)
			val td2 = TDigest.sketch(poissonData, maxDiscrete = MAX_DISCRETE)

			// NOTE: so skewed that we need to tweak the max discrete parameter to get convergence for KSD
			val tdCombine = TDigest.combine(td1, td2, maxDiscrete = MAX_DISCRETE)

			kolmogorovSmirnovD(tdCombine, PoissonDist(2)) should beLessThanTuple(EPSILON_T)
		}

		"---> combine multiple sketches" in {
			//TEST_ID = (1, 'b')

			val poissonDist: PoissonDist = PoissonDist(1)

			val data: Seq[Seq[Int]] = Seq.fill[Seq[Int]](1 + NUM_MONOIDAL_ADDITIONS)(
				poissonDist.sample(SAMPLE_SIZE)
			)

			// Computing the T-Digest sketches, cumulatively, keeping track of the previous ones.
			//  Means; sum first 2, sum first 3, sum first 4, sum first 5, ... and keep track, all the way up to
			//  NUM_MONOIDAL_ADDITIONS.
			val manyTDSketches: Seq[TDigest] = data
				.map((distSmp: Seq[Int]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
				.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) =>
					TDigest.combine(ltd, rtd, maxDiscrete = MAX_DISCRETE))
				.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

			// Computing the KSD statistic
			val ksdsCumulative: Seq[(Double, Double)] = manyTDSketches
				.map((td: TDigest) => kolmogorovSmirnovD(td, poissonDist))

			println(s"ksdsCumulative = $ksdsCumulative")
			//KSD(tdCombine, GammaDist(2, 8)) should beLessThanTuple(EPS)
			// First ones won't be below epsilon maybe .. check just the last 5? Where do they start to go below
			// the EPSILON?  Then graph???
			ksdsCumulative
				.drop(NUM_MONOIDAL_ADDITIONS - 5)
				.map(ksd => ksd should beLessThanTuple(EPSILON_T)) // checking the last few are converging to below
			// the
			// EPSILON


		}

	}*/
}
