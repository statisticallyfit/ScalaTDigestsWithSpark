package Project_IsarnSketches

import org.apache.commons.math3.distribution.IntegerDistribution
import org.apache.commons.math3.distribution.{GammaDistribution, GeometricDistribution, GumbelDistribution, PoissonDistribution}
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


object KolmogorovTryout {

	val N = 10000

	def generateShortRange(dist: IntegerDistribution) = {
		val xmin = dist.inverseCumulativeProbability(0).toDouble
		val xmax = dist.inverseCumulativeProbability(1).toDouble

		val step = (xmax - xmin) / N.toDouble // TODO why n = 1000? To change?

		val xvals = xmin until xmax by step

		xvals.map(x => dist.cumulativeProbability(x.toInt))
			.takeWhile(x => x != 1.0) :+ 1.0
	}

	def generateLongRange(dist: IntegerDistribution) = {
		val xmin = dist.inverseCumulativeProbability(0).toDouble
		val xmax = 1000.0

		val step = (xmax - xmin) / N.toDouble

		val xvals = xmin until xmax by step

		xvals.map(x => dist.cumulativeProbability(x.toInt))
			.takeWhile(x => x != 1.0) :+ 1.0
	}


	def main(args: Array[String]) {
		println(generateShortRange(new PoissonDistribution(3)))
		println(generateLongRange(new PoissonDistribution(3)))
	}

}
