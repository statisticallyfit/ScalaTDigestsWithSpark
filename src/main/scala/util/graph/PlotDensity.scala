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
import util.distributionExtensions.instances.AllInstances._
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
				strokeWidth = Some(3.0)
			)),
			xbounds = Some(Bounds(distXMin, distXMax)) // NOTE necessary to include x bounds or graphs WON'T appear
		)
	}


	def plotDensities[T: TypeTag, D](dists: Seq[Distr[T, D]],
							 titleName: Option[String] = None,
							   HOW_MANY: Option[Int] = Some(5),
							 givenColorSeq: Option[Seq[Color]] = None)(implicit evNum: Numeric[T],
										    evSamp: Sampling[T, Distr[T, D]], // TODO change to T, D
										    evProb: ProbabilityFunction[T, D]): Unit = {

		val indexedDists: Seq[(Int, Distr[T, D])] = dists.indices.zip(dists)

		val sampleData: Seq[Double] = dists.flatMap(_.sample(SAMPLE_SIZE)).map(evNum.toDouble(_))
		val (xMIN, xMAX): (Double, Double) = (sampleData.min, sampleData.max)

		val howManyToShow: Int = HOW_MANY.isDefined match {
			case false => indexedDists.length
			case true => indexedDists.length <= HOW_MANY.get match {
				case true => indexedDists.length
				case false => HOW_MANY.get
			}
		}

		val step: Int = scala.math.ceil(indexedDists.length * 1.0 / howManyToShow).toInt
		val shorterDists = indexedDists.filter{ case (idx, _) => idx % step == 0}

		val colorSeq: Seq[Color] = givenColorSeq.isDefined match {
			case false => Color.getGradientSeq(shorterDists.length)
			case true => givenColorSeq.get
		}
		val densityPlots: Seq[Plot] = shorterDists.unzip._2
			.zip(colorSeq).map{ case (d, c) => getDensity(d, c)}

		val plt: Drawable = Overlay(densityPlots: _*)
			.xAxis()
			.yAxis()
			.xbounds(lower = xMIN, upper = xMAX)
			.title(titleName.getOrElse(""))
			.standard() //.frame()
			.overlayLegend() // for name labels to appear
			.xLabel("x")
			.yLabel("y").render()

		displayPlot(plt)
	}
}
