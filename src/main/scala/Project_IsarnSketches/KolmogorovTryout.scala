package Project_IsarnSketches

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
import util.distributionExtensions.syntax._

/**
 * TODO: genate values from 0 -> infinity (cdf fro this range) and compare to cdf of values from 0 -> 1 (cdfinversed)
 * -- see the isarn-sketches-spark KSD function.
 * COMPARE:
 * -- length of the cdf arrays
 * -- difference of the cdf arrays (subtract)
 * -- draw the cdf arrays using evilplot (density) to see if the intermediate points differ in space or just in width
 * of each other
 *
 * GOAL: to see if i can use the longer range (0 -> infinity) to generate the x-vals to compute the discrete cdf
 * values because if i use the smaller range (cdfInv(0) -> cdfInv(1)) then the discrete cdf only allows the discrete
 * numbers and the cdf array gets a lot smaller, thus causing less data for the tdigest test in isarn-sketches-spark,
 * and is why the discrete dists have smaller-sized tdigest arrays than the continuous ones.
 */
import scala.reflect.runtime.universe._
import scala.math.Numeric.Implicits._
import util.GeneralUtil

object KolmogorovTryout {

	val N = 10000



	// Assume: T == Integer (for integer distribution)
	def generateShortRange[/*T: Numeric: TypeTag, */D](dist: DiscreteDist[D])
											(implicit evCdf: CDF[Int, DiscreteDist[D]]): Seq[Double]
	= {
		val xmin = dist.inverseCdf(0)
		val xmax = dist.inverseCdf(1) - 100000

		//val evNum = implicitly[Numeric[T]]
		//val step = (xmax - xmin) / N.toDouble

		val xvals: Seq[Int] = GeneralUtil.generateTSeq[Int](xmin, xmax)

		xvals
			.map(x => dist.cdf(x))
			.takeWhile(x => x != 1.0) :+ 1.0
	}

	def generateLongRange[D](dist: DiscreteDist[D])(implicit evCdf: CDF[Int, DiscreteDist[D]]): Seq[Double] = {
		val xmin = dist.inverseCdf(0)
		val xmax = 1000

		//val step = (xmax - xmin) / N.toDouble

		val xvals: Seq[Int] = GeneralUtil.generateTSeq[Int](xmin, xmax)

		xvals.map(x => dist.cdf(x))
			.takeWhile(x => x != 1.0) :+ 1.0
	}


	def main(args: Array[String]) {
		println(generateShortRange(PoissonDist(3)))
		println(generateLongRange(PoissonDist(3)))
	}

}
