package util.graph


// Plotting imports
import com.cibo.evilplot._
import com.cibo.evilplot.colors.{CategoricalColoring, Color, Coloring, GradientMode, HTMLNamedColors}
import com.cibo.evilplot.geometry.{Drawable, EmptyDrawable, LineStyle, Text}
import com.cibo.evilplot.numeric.{Bounds, Point}
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.{BarRenderer, PathRenderer, PointRenderer}

import smile.stat.distribution.{KernelDensity, Mixture}

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._


/**
 *
 */
object PlotDensity {


	final val SAMPLE_SIZE: Int = 8000 //50000 // fifty thousand


	def getDensity[T: TypeTag, D](dist: Distr[T, D],
							distColor: Color)(implicit evNum: Numeric[T],
										   evSamp: Sampling[T, Distr[T, D]], // TODO change to T, D
										   evProb: ProbabilityFunction[T, D]): Plot	= {

		val makeDensity: T => Double = x => dist.probabilityFunction(x)
		//evProb.prob(dist.getDist, x)
		//dist.getDist.p(evNum.toInt(x))

		val rawDoubleToT: Double => T = xDouble => typeOf[T].toString.split('.').last match {
			case "IntZ" => BigInt(xDouble.toInt).asInstanceOf[T]
			case "Real" => BigDecimal.valueOf(xDouble).asInstanceOf[T]
		}

		// Get xbounds or else the graph won't appear!
		val sampleDist: Seq[Double] = dist.sample(SAMPLE_SIZE).map(evNum.toDouble(_))
		val (distXMin, distXMax): (Double, Double) = (sampleDist.min, sampleDist.max)

		FunctionPlot(
			function = (x:Double) => makeDensity(rawDoubleToT(x)),
			pathRenderer = Some(PathRenderer.default(
				color = Some(distColor),
				label = Text(msg = dist.getDist.toString),
				strokeWidth = Some(5.0)
			)),
			xbounds = Some(Bounds(distXMin, distXMax)) // NOTE necessary to include x bounds or graphs WON'T appear
		)
	}
}
