package utilTest


import org.isarnproject.sketches.TDigest
import org.specs2.matcher.{Expectable, Matcher, MatchersImplicits, ShouldMatchers}
import org.specs2.mutable._
import util.GeneralUtil
import util.GeneralUtil.numToT
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._


import utilTest.TestData._

/**
 *
 */


object TestTools  {


	object SpecsTools extends ShouldMatchers with  MatchersImplicits {

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

	object GeneralTools {
		// The count of elements to take of the tail of the list
		final val NUM_REPEAT = 6

		// increase each element by this multiple (1, 2, 4, 8, 16, 32 ...) so for instance if you have elemnts (n1,
		// n2,n3,n4,n5...), and NUM_INCR = 2, then result is (n1, n2,n2, n3,n3,n3,n3, n4,n4,n4,n4,n4,n4,n4, ...)
		final val NUM_INCR = 2

		def repeatTail[T](list: Seq[T], numReps: Int = NUM_REPEAT, numIncr: Int = NUM_INCR): Seq[T] = {

			// step 1: take the last NUM_REPEAT from end of list
			val elemsToRepeat: Seq[T] = list.drop(list.length - numReps)

			// step 2: repeat each one like a cone
			val reps = (1 to numReps).map(_ * numIncr)
			val repeatedEnds: Seq[T] = elemsToRepeat.zip(reps).map(ar => ar match {case (a, r) => List.fill(r)(a)})
				.flatten

			// step 3: combine into list
			val elemsFront: Seq[T] = list.take(list.length - numReps)

			elemsFront ++ repeatedEnds
		}

		// Task 1: repeat ends  + combine
		// Task 2: interweave betas to elonagate further


		/**
		 * Splits a list like [(g1, 10), (g2, 10), (g3, 10), (g4, 20), (g5, 20), ...] into a list of lists
		 * separated by the second element in the tuple:
		 * [[(g1, 10), (g2, 10), (g3, 10)], [(g4, 20), (g5, 20)], [], ...]
		 * @param lst
		 * @tparam T - the type of the second element in the tuple, the one by which the tuples are grouped into
		 *           lists.
		 * @tparam D - the type of the first element in the tuple
		 * @return
		 */
		def splitGroups[T: Numeric, D](lst: Seq[(D, T)]): Seq[Seq[(D, T)]] = {
			def splitter(acc: Seq[Seq[(D, T)]], processList: Seq[(D, T)]): Seq[Seq[(D, T)]] = {
				if(processList.isEmpty){
					return acc
				} else {
					if(acc.isEmpty){
						splitter(List(List(processList.head)), processList.tail)
					}
					else if(processList.head._2 == acc.last.head._2){ // , tag onto end off last one in accumulator
						splitter(acc.init :+ (acc.last :+ processList.head), processList.tail)
					} else { // else the split happens here [n2, n2, ...] vs. [[n1,n1], ...]
						splitter(acc :+ List(processList.head), processList.tail)
					}
				}
			}
			splitter(List(), lst)
		}
	}


	object StatTools {

		def calcMode[T: Numeric, D](dist: Distr[T, D]): Double = {
			dist.getDist match {
				case g: GammaDist => (g.shape - 1) / g.scale
				case g: GumbelDist => (g.mu)
				case n: NormalDist => n.mu
				case cu: ContinuousUniformDist => ??? //TODO all x in [a, b] but how to give that?
				case e: ExponentialDist => 0
				case w: WeibullDist => {
					if(w.alphaShape == 1) 0
					else {
						val (a, b) = (w.alphaShape, w.betaScale)
						scala.math.pow(1.0 * b * ((a - 1) / (a * 1.0)),  (1.0 / a))
					}
				}

				case p: PoissonDist => scala.math.floor(p.lambda) // TODO and mode is (mu, mu-1) if mu is
				// positive integer
				case b: BinomialDist => scala.math.floor((b.numTrials + 1) * b.p)
				case g: GeometricDist => 1

			}
		}


		// Does the test by samples generated from the digest and other distribution
		// NOTE: same as isarn-sketches `testSamplingPDF` and `testSamplingPMF` = https://github.com/isarn/isarn-sketches/blob/develop/src/test/scala/org/isarnproject/sketches/TDigestTest.scala#L51-L70

		def kolmogorovSmirnovSampleD[T: Numeric : TypeTag, D](tdgst: TDigest, dist: Distr[T, D])
												   (implicit evCdf: CDF[T, Distr[T, D]],
												    evSmp: Sampling[T, Distr[T, D]]): Double = {

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
												 dist: Distr[T, D],
												 //testID: (Int, Char) = TEST_ID,
												 n: Int = 10000)
												(implicit evCdf: CDF[T, Distr[T, D]]): Double = {
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

		def kolmogorovSmirnovD[T: Numeric : TypeTag, D](tdgst: TDigest, dist: Distr[T, D])
											  (implicit evCdf: CDF[T, Distr[T, D]],
											   evSmp: Sampling[T, Distr[T, D]]): (Double, Double) = {

			val ksdCdf = kolmogorovSmirnovCdfD(tdgst, dist)
			val ksdSample = kolmogorovSmirnovSampleD(tdgst, dist)

			(ksdCdf, ksdSample)

		}



		//def beLessThanTuple[T: Numeric](right: (T, T)): Matcher[(T,T)] = be_<=(right._1) and be_<=(right._2)
		//def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)
		//def beShort1: AnyRef with Matcher[Any] = be_<=(5) ^^ { (t: Any) => t.toString.size }



		// Does something similar to KSD test except doesn't use absolute value --- leaves the signs as they are in
		//  order to see which cdf is "underneath" or "above" the other (if result < 0, then cdfA < cdfB, else if result
		//  > 0 then cdfA > cdfB) and we can thus tell which one is "shifted" left or right, respectively
		def cdfSignTest[T: Numeric : TypeTag, D](dist1: Distr[T, D], dist2: Distr[T, D], n: Int = 10000)
										(implicit evCdf: CDF[T, Distr[T, D]],
										 smpl: Sampling[T, Distr[T, D]]): Double = {
			// TODO require(tdgst.nclusters > 1) // size == num clusters //require(tdgst.size() > 1)
			require(n > 0)


			// Create the t-digest sketches out of the distribution samples
			val td1: TDigest = TDigest.sketch(dist1.sample(SAMPLE_SIZE_TEST), maxDiscrete = MAX_DISCRETE)
			val td2: TDigest = TDigest.sketch(dist2.sample(SAMPLE_SIZE_TEST), maxDiscrete = MAX_DISCRETE)

			import scala.math.{min, max}

			val (xmin1, xmax1) = (td1.cdfInverse(0), td1.cdfInverse(1))
			val (xmin2, xmax2) = (td2.cdfInverse(0), td2.cdfInverse(1))
			val (xmin, xmax) = (min(xmin1, xmin2), max(xmax1, xmax2))

			val (xmin1_T, xmin2_T, xmax1_T, xmax2_T): (T, T, T, T) = (numToT(xmin1), numToT(xmin2), numToT(xmax1),
				numToT(xmax2))

			//val xvals1: Seq[T] = GeneralUtil.generateTSeqFromDouble[T](xmin1, xmax1)
			//val xvals2: Seq[T] = GeneralUtil.generateTSeqFromDouble[T](xmin2, xmax2)
			val xvals: Seq[T] = GeneralUtil.generateTSeqFromDouble[T](xmin, xmax)

			/*val tdCdf: T => Double = d => {
				val dd: Double = new java.lang.Double(implicitly[Numeric[T]].toDouble(d))

				if (tdgst.nclusters <= tdgst.maxDiscrete) tdgst.cdfDiscrete(dd) else tdgst.cdf(dd)
			}*/



			import Ordering.Implicits._

			// Calculates the KSD statistic number here:
			val signCdfDiffs: Seq[Double] = xvals
				//.iterator
				.map(x => dist1.cdf(x) - dist2.cdf(x) /*(if(xmin1 < xmin2) {
						if((xmin1_T <= x) && (x < xmin2_T)) dist1.cdf(x)
						else (dist1.cdf(x) - dist2.cdf(x)) // xmin2_T < x
					} else if(xmin1 > xmin2) {
						if((xmin2_T <= x) && (x < xmin1_T)) dist2.cdf(x)
						else (dist2.cdf(x) - dist1.cdf(x)) // xmin1_T < x
					} else {// xmin1 == xmin2
						dist1.cdf(x) - dist2.cdf(x)
						/*if(xmin1_T == x) (dist1.cdf(x) - dist2.cdf(x))
						else if(x > xmin1_T) (dist1.cdf(x) - dist2.cdf(x))*/
					}).asInstanceOf[Double]*/
				) //math.abs(tdCdf(x) - dist.cdf(x)))

			val signKSD: Double = signCdfDiffs.sum

			signKSD
		}
	}



}
