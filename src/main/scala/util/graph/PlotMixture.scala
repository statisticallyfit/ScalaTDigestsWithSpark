package util.graph



// Plotting imports
import com.cibo.evilplot._
import com.cibo.evilplot.colors.{CategoricalColoring, Color, Coloring, GradientMode, HTMLNamedColors}
import com.cibo.evilplot.geometry.{Drawable, EmptyDrawable, LineStyle, Text}
import com.cibo.evilplot.numeric.{Bounds, Point}
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.{BarRenderer, PathRenderer, PointRenderer}

import com.manyangled.snowball.analysis.interpolation.MonotonicSplineInterpolator
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction

import smile.stat.distribution.{KernelDensity, Mixture}

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._


/**
 *
 */
object PlotMixture {


	/**
	 *
	 * @param sampleData the sample data of each of the mixture components, flattened into one array
	 * @param canonParamMix the canonical mixture object (parametric) --- is made of the original distributions with
	 *                      the original parameters
	 * @param estParamMix the estimated mixture object from data (parametric) --- has estimated the original
	 *                    parameters based off data, mirroring the canonical mixture object's parameters
	 * @param kernelNonParamMix the estimated kernel density mixture object from data (non-parametric)
	 */
	def plotMixtureDist(sampleData: Array[Double], canonicalMixture: Mixture, estimatedMixture: Mixture,
					kernelMixture: KernelDensity,
					titleName: Option[String] = None): Unit = {

		// generate data from canonical mixture:
		val NUM_POINTS: Int = 2000

		val (xMIN, xMAX): (Int, Int) = (sampleData.min.toInt, sampleData.max.toInt)
		println(s"xmin, xmax = $xMIN, $xMAX")

		// Create histogram of data (from sample)
		val plotHistData: Seq[Double] => Plot = data => Histogram(
			data,
			barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.blueViolet.copy(opacity = 0.25)))),
			binningFunction = Histogram.density,
			xbounds = Some(Bounds(xMIN, xMAX)) // find the xbounds
		)
		val histPlot: Plot = plotHistData(sampleData)

		// Create the density estimates
		val colors: Seq[Color] = Color.getGradientSeq(3) // for the three probs lists above
		val funcs: Seq[Double => Double] = Seq(
			x => canonicalMixture.p(x),
			x => estimatedMixture.p(x),
			x => kernelMixture.p(x)
		)
		val funcLabels: Seq[String] = Seq(
			"canonical mixture",
			"estimated mixture",
			"kernel density"
		)
		/*val colorFuncLabelTriple: List[(Color, Double => Double, String)] =
			List(colors, funcs, funcLabels).transpose.map{case List(c, f, l)	=> (c, f,	l)}*/

		val densityPlots: Seq[Plot] = //Overlay(
			colors.zip(funcs).zip(funcLabels).map { case ((color, mixtureFunc), labelFunc) =>
				FunctionPlot.series(
					function = mixtureFunc,
					numPoints = Some(NUM_POINTS), // to be bigger than default of 800
					xbounds = Some(Bounds(xMIN, xMAX)),
					name = labelFunc,
					color = color
				)
			}

		displayPlot(Overlay((histPlot +: densityPlots): _*)
			.standard()
			.overlayLegend() // for name labels to appear
			.title(titleName.getOrElse(""))
			.xbounds(xMIN, xMAX) // args for the function
			.render()
		)
	}
}
