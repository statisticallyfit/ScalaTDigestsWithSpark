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

/**
 *
 */

class CombineSameDistributionSpecs extends Specification {

	" (Continuous: Gamma) TDigest can combine sketches to yield the same distribution" should {

		"---> combine once" in {
			//TEST_ID = (1, 'a')

			val gammaData: Seq[Double] = GammaDist(2, 8).sample(SAMPLE_SIZE_TEST)
			//List.fill[Double](SAMPLE_SIZE){ GammaDist(2,8).sample }

			val td1 = TDigest.sketch(gammaData, maxDiscrete = MAX_DISCRETE)
			val td2 = TDigest.sketch(gammaData, maxDiscrete = MAX_DISCRETE)

			val tdCombine = TDigest.combine(td1, td2)

			//kolmogorovSmirnovSampleD(tdCombine, GammaDist(2, 8)) should beLessThanTuple(EPSILON)
			//kolmogorovSmirnovCdfD(tdCombine, GammaDist(2, 8)) should beLessThanTuple(EPSILON)
			kolmogorovSmirnovD(tdCombine, GammaDist(2, 8)) should beLessThanTuple(EPSILON_T)
		}

		"---> combine multiple sketches" in {
			//TEST_ID = (1, 'b')

			val gammaDist: GammaDist = GammaDist(3, 9)

			val data: Seq[Seq[Double]] = Seq.fill[Seq[Double]](1 + NUM_MONOIDAL_ADDITIONS)(
				gammaDist.sample(SAMPLE_SIZE_TEST)
			)

			// Computing the T-Digest sketches, cumulatively, keeping track of the previous ones.
			//  Means; sum first 2, sum first 3, sum first 4, sum first 5, ... and keep track, all the way up to
			//  NUM_MONOIDAL_ADDITIONS.
			val manyTDSketches: Seq[TDigest] = data
				.map((distSmp: Seq[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
				.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd))
				.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

			// Computing the KSD statistic
			/*val ksdsCdfCumul: Seq[Double] = manyTDSketches
				.map((td: TDigest) => kolmogorovSmirnovCdfD(td, gammaDist))
			val ksdsSampleCumul: Seq[Double] = manyTDSketches
				.map((td: TDigest) => kolmogorovSmirnovSampleD(td, gammaDist))

			println(s"ksds (cdf) = $ksdsCdfCumul")
			println(s"ksds (sample) = $ksdsSampleCumul")*/

			val ksdsCumulative: Seq[(Double, Double)] = manyTDSketches
				.map((td: TDigest) => kolmogorovSmirnovD(td, gammaDist))

			println(s"ksds cumul = $ksdsCumulative")
			//KSD(tdCombine, GammaDist(2, 8)) should beLessThanTuple(EPS)
			// First ones won't be below epsilon maybe .. check just the last 5? Where do they start to go below
			// the EPSILON?  Then graph???
			ksdsCumulative
				.drop(NUM_MONOIDAL_ADDITIONS - 5)
				.map(ksd => ksd should beLessThanTuple(EPSILON_T))

		}

	}

	" (Discrete: Poisson) TDigest can combine sketches to yield the same distribution" should {

		"---> combine once" in {
			//TEST_ID = (1, 'a')

			// NOTE: the lower the mean parameter, the more right-skewed. As lambda -> infinity, poisson -> normal
			//  (rule? check)
			val poissonData: Seq[Int] = PoissonDist(2).sample(SAMPLE_SIZE_TEST)
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
				poissonDist.sample(SAMPLE_SIZE_TEST)
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

	}

}
