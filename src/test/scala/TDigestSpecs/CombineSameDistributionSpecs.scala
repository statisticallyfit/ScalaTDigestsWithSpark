package TDigestSpecs


import TDigestSpecs.TestData.{EPSILON, NUM_MONOIDAL_ADDITIONS, SAMPLE_SIZE, TEST_ID}
import TDigestSpecs.TestTools.kolmogorovSmirnovCdfD
//import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest
import org.isarnproject.sketches.TDigest
//import org.isarnproject.sketches.java.TDigest

import org.specs2.matcher.{Matcher, MatchersImplicits, Expectable, ShouldMatchers}
import org.specs2.mutable._

import util.GeneralUtil
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._

/**
 *
 */
object TestData {
	final val SAMPLE_SIZE: Int = 10000

	// Number of times to combine the t-digests
	final val NUM_MONOIDAL_ADDITIONS = 20

	// Kolmogorov Smirnov epsilon limit bound
	final val EPSILON: Double = 0.02 // value copied from isarn-sketches-spark tests
	final val EPSILON_T: (Double, Double) = (EPSILON, EPSILON)

	//Simple identifier to keep track of which test is running
	var TEST_ID: (Int, Char) = (0, 'a')

	// Max discrete parameter - the higher, the more discrete estimates are included in the discrete distribution
	// t-sketch estimates
	// TODO find out exactly algorithmically what it means
	final val MAX_DISCRETE = 500 // same value as placed in the isarn-sketches-spark test to make discrete
	// distribution t-sketches pass (otherwise not enough t-sketch discrete estimates are included so KSD can't
	// converge)

}

import TestData._

object TestTools  extends ShouldMatchers with MatchersImplicits {

	// Does the test by samples generated from the digest and other distribution
	// NOTE: same as isarn-sketches `testSamplingPDF` and `testSamplingPMF` = https://github.com/isarn/isarn-sketches/blob/develop/src/test/scala/org/isarnproject/sketches/TDigestTest.scala#L51-L70

	def kolmogorovSmirnovSampleD[T: Numeric: TypeTag, D](tdgst: TDigest, dist: Dist[T, D])
											  (implicit evCdf: CDF[T, Dist[T, D]],
											   evSmp: Sampling[T, Dist[T, D]]): Double = {

		// Sample from the t-digest sketch of this distribution
		val tdSamples: Array[Double] = typeOf[T].toString contains "Int" match { // Int or IntZ
			case true => Array.fill(SAMPLE_SIZE) {tdgst.samplePMF} // discrete dist
			case false => Array.fill(SAMPLE_SIZE) {tdgst.samplePDF} // real continuous dist
		}

		// Sample from the dist itself
		val distSamples: Seq[T] = dist.sample(SAMPLE_SIZE)
		val evNum: Numeric[T] = implicitly[Numeric[T]]
		val distSamplesDouble: Array[Double] = distSamples.map(s => evNum.toDouble(s)).toArray

		val kst = new org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest()
		val d: Double = kst.kolmogorovSmirnovStatistic(tdSamples, distSamplesDouble)


		// debug
		println(s"ksd (sample) = $d")

		d
	}



	// Does the test by cdf
	def kolmogorovSmirnovCdfD[T: Numeric : TypeTag, D](tdgst: TDigest,
											 dist: Dist[T, D],
											 //testID: (Int, Char) = TEST_ID,
											 n: Int = 10000)
											(implicit evCdf: CDF[T, Dist[T, D]]): Double = {
		require(tdgst.nclusters > 1) // size == num clusters //require(tdgst.size() > 1)
		require(n > 0)


		/*val xmin: T = dist.inverseCdf(0) // TODO
		val xmax: T = typeOf[T].toString.split('.').last match {
			case "IntZ" => BigInt(1000000).asInstanceOf[T]
			case "Real" => BigDecimal.valueOf(100000.0).asInstanceOf[T]
		}*/
		//val xmax: T = dist.inverseCdf(1) // TODO weird error BigDecimal numberformatException here -- why? too big?

		val xmin: Double = tdgst.cdfInverse(0) // x-value at beginning (total area = 0)
		val xmax: Double = tdgst.cdfInverse(1) //10000.0

		val xvals: Seq[T] = GeneralUtil.generateTSeqFromDouble[T](xmin, xmax)

		val tdCdf: T => Double = d => {
			val dd: Double = new java.lang.Double(implicitly[Numeric[T]].toDouble(d))

			if (tdgst.nclusters <= tdgst.maxDiscrete) tdgst.cdfDiscrete(dd) else tdgst.cdf(dd)
		}


		// Calculates the KSD statistic number here:
		val ksd: Double  = xvals
			.iterator
			.map(x => math.abs(tdCdf(x) - dist.cdf(x)))
			.max

		println("----------------------------------------------------------------------------------------")
		//println(s"Test #$TEST_ID")
		println(s"Distribution = ${dist.toString} |  typeOf[T] = ${typeOf[T].toString.split('.').last}")
		println(s"xmin = $xmin, xmax = $xmax)  |  xvals = [${xvals.take(5)}, ..., ${xvals.drop(xvals.length - 5)}]")
		println(s"xvals.length = ${xvals.length}")
		println(s"tdigest size = ${tdgst.nclusters}") //size
		println(s"ksd (cdf) = $ksd")
		println("----------------------------------------------------------------------------------------")

		ksd
	}

	def kolmogorovSmirnovD[T: Numeric: TypeTag, D](tdgst: TDigest, dist: Dist[T, D])
										 (implicit evCdf: CDF[T, Dist[T, D]],
										  evSmp: Sampling[T, Dist[T, D]]): (Double, Double) = {

		val ksdCdf = kolmogorovSmirnovCdfD(tdgst, dist)
		val ksdSample = kolmogorovSmirnovSampleD(tdgst, dist)

		(ksdCdf, ksdSample)

	}



	//def beLessThanTuple[T: Numeric](right: (T, T)): Matcher[(T,T)] = be_<=(right._1) and be_<=(right._2)
	//def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)
	//def beShort1: AnyRef with Matcher[Any] = be_<=(5) ^^ { (t: Any) => t.toString.size }

	// NOTE code sources
	// https://stackoverflow.com/questions/36433390/negating-a-custom-matcher-in-specs2
	// https://gist.github.com/betandr/e5021bae3ac133e33b5ec6e1df30f782
	def beLessThanTuple(right: (Double, Double)) = new Matcher[(Double, Double)] {
		def apply[S <: (Double, Double)](leftInExpectable: Expectable[S]) = {
			val left: S = leftInExpectable.value

			result((left._1 <= right._1) && (left._2 <= right._2), "pass", "fail", leftInExpectable)
		}
	}

}
import TestTools._

class TSketchCombineSpecs extends Specification {

	" (Continuous: Gamma) TDigest can combine sketches to yield the same distribution" should {

		"---> combine once" in {
			//TEST_ID = (1, 'a')

			val gammaData: Seq[Double] = GammaDist(2, 8).sample(SAMPLE_SIZE)
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
				gammaDist.sample(SAMPLE_SIZE)
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

			(0.0001, 0.05) should beLessThanTuple((0.02, 0.02))

		}

	}

	// TODO make sure to follow above template for a discrete distribution BUT also place the MAX_DISCRETE parameter
	//  when declaring the T-sketch! (else no convergence)

	/*"TDigest can combine sketches of different distributions" should {

		" ---> combine once: ..." in {
			TEST_ID = (2, 'a')
		}

		"---> combine multiple sketches: can force a new parameter for the same distribution (CONCEPT DRIFT) "{
			TEST_ID = (2, 'b')
		}

		"---> combine multiple sketches: can force a new shape (DISTRIBUTION SHIFT)" in {
			TEST_ID = (2, 'c')
		}
	}*/

}
