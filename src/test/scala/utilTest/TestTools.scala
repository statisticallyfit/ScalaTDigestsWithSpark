package utilTest


import org.isarnproject.sketches.TDigest
import org.specs2.matcher.{Expectable, Matcher, MatchersImplicits, ShouldMatchers}
import org.specs2.mutable._
import util.GeneralUtil
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._

import utilTest.TestData._

/**
 *
 */


object TestTools extends ShouldMatchers with MatchersImplicits {

	// Does the test by samples generated from the digest and other distribution
	// NOTE: same as isarn-sketches `testSamplingPDF` and `testSamplingPMF` = https://github.com/isarn/isarn-sketches/blob/develop/src/test/scala/org/isarnproject/sketches/TDigestTest.scala#L51-L70

	def kolmogorovSmirnovSampleD[T: Numeric : TypeTag, D](tdgst: TDigest, dist: Dist[T, D])
											   (implicit evCdf: CDF[T, Dist[T, D]],
											    evSmp: Sampling[T, Dist[T, D]]): Double = {

		// Sample from the t-digest sketch of this distribution
		val tdSamples: Array[Double] = typeOf[T].toString contains "Int" match { // Int or IntZ
			case true => Array.fill(SAMPLE_SIZE_LARGE) {
				tdgst.samplePMF
			} // discrete dist
			case false => Array.fill(SAMPLE_SIZE_LARGE) {
				tdgst.samplePDF
			} // real continuous dist
		}

		// Sample from the dist itself
		val distSamples: Seq[T] = dist.sample(SAMPLE_SIZE_LARGE)
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
		val ksd: Double = xvals
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

	def kolmogorovSmirnovD[T: Numeric : TypeTag, D](tdgst: TDigest, dist: Dist[T, D])
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
