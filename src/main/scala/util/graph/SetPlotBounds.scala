package util.graph

import org.apache.commons.math3.distribution.{IntegerDistribution, RealDistribution}

import scala.reflect.runtime.universe._


import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._
/**
 *
 */

object Data {
	final val SAMPLE_SIZE = 8000
}
import Data._

object SetPlotBounds {


	def getXBounds[T: TypeTag, D](dists: Seq[Distr[T, D]])(implicit evNum: Numeric[T],
												evSmp: Sampling[T, D]): (Double, Double) = {

		/*val posInf: T = typeOf[T].toString.split('.').last match {
			case "IntZ" => BigInt(Integer.MAX_VALUE).asInstanceOf[T]
			case "Real" => BigDecimal.valueOf(Double.MaxValue).asInstanceOf[T]
		}
		val negInf: T = typeOf[T].toString.split('.').last match {
			case "IntZ" => BigInt(Integer.MIN_VALUE).asInstanceOf[T]
			case "Real" => BigDecimal.valueOf(Double.MinValue).asInstanceOf[T]
		}*/

		val sampleData: Seq[Double] = dists.flatMap(_.sample(SAMPLE_SIZE)).map(evNum.toDouble(_))

		var (xmin, xmax) = (sampleData.min, sampleData.max)

		typeOf[T].toString.split('.').last match {
			case "IntZ" => {
				println(s"INSIDE INTZ for ${dists.map(_.toString)}")

				val intDists: Seq[IntegerDistribution] = dists.map(d => d.getDist
					.asInstanceOf[IntegerDistribution])

				val (lowerBounds, upperBounds): (Seq[Int], Seq[Int]) = intDists
					.map(d => (d.getSupportLowerBound, d.getSupportUpperBound))
					.unzip

				if (lowerBounds.min > sampleData.min){
					xmin = lowerBounds.min
				}
				if (upperBounds.max < sampleData.max) {
					xmax = upperBounds.max
				}
			}
			case "Real" => {
				println(s"INSIDE REAL for ${dists.map(_.toString)}")

				val realDists: Seq[RealDistribution] = dists.map(d => d.getDist
					.asInstanceOf[RealDistribution])

				val (lowerBounds, upperBounds): (Seq[Double], Seq[Double]) = realDists
					.map(d => (d.getSupportLowerBound, d.getSupportUpperBound))
					.unzip

				if (lowerBounds.min > sampleData.min){
					xmin = lowerBounds.min
				}
				if (upperBounds.max < sampleData.max) {
					xmax = upperBounds.max
				}
			}

		}

		(xmin, xmax)
	}

}
