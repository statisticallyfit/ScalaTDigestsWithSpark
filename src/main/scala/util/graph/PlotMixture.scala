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
import smile.stat.distribution._
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._

import scala.reflect.runtime.universe._

import scala.language.implicitConversions
import util.ConvertMyDistToSmileDist._
import flip.pdf.Sketch
import flip.implicits._
import util.graph.SetPlotBounds.getXBounds

/**
 *
 */
object PlotMixtureData {
	final val SAMPLE_SIZE: Int = 8000 //50000 // fifty thousand
}
import PlotMixtureData._

object PlotMixture {


	/**
	 *
	 * @param lastSketchPotentialMixture = the last sketch generated from a group of sketches
	 * @param originalDists = dists used to make the sketches (each sketch is found from estimating from the sampled
	 *                      data from each dist)
	 *
	 * Rturn: the canonical and estimated mixture plot objects
	 */
	def getMixtureTrueEstimated[T: TypeTag, D](lastSketchPotentialMixture: Sketch[Double],
					 originalDists: Seq[Distr[T, D]])(implicit evSamp: Sampling[T, D],
											    evNum: Numeric[T]): (Plot, Option[Plot]) = {

		val conceptDriftData: Array[Double] = Array.fill[Double](SAMPLE_SIZE){ lastSketchPotentialMixture.sample._2 }

		// Create the canonical mixture model (against which to compare to the estimated one at the end)
		// TESTING 1: add same probability to all the gammas
		// TESTING 2: add increasing probability to the ending gammas

		// Create a priori value (set equal probability to all dists)
		val priori: Double = 1.0 / originalDists.length
		val components: Seq[Mixture.Component] = originalDists.map(dist =>
			new Mixture.Component(priori, dist.toSmileAbsDist)
		)
		val canonicalMixture: Mixture = new Mixture(components:_*)

		// Estimate the mixture model
		/*val sampleDistData: Seq[Double] = originalDists.flatMap(dst => dst.sample(SAMPLE_SIZE)).map(t => evNum
			.toDouble(t))

		// TODO error here - must check if the dists type indeed implement the exponentialfamily in smile lib -- or
		//  do cach error when casting to expfamily
		//components(i) = (components(i).distribution.asInstanceOf[ExponentialFamily]).M(x, posteriori(i))
		// then if error just keep the canonicalmixt only and dont fit the estinate
		// return type (plot, option[plot])
		val estimatedMixture = ExponentialFamilyMixture.fit(
			sampleDistData.toArray,
			components:_*
		)*/

		// Plot the canonical mixture

		// Just choose any of the dists because they are the same type and have the same support
		// TODO update later when having many different dists with different support bounds  - need to pass a seq
		//  of dists and get a min / max of their lower/upper bound supports.
		val (xMIN, xMAX): (Double, Double) = getXBounds(originalDists)// (sampleData.min, sampleData.max)


		val canonPlot: Plot = FunctionPlot(
			function = (x:Double) => canonicalMixture.p(x),
			pathRenderer = Some(PathRenderer.default(
				color = Some(HTMLNamedColors.black),
				label = Text(msg = "Canonical mixture"),
				strokeWidth = Some(3.0)
			)),
			xbounds = Some(Bounds(xMIN, xMAX)) // NOTE necessary to include x bounds or graphs WON'T appear
		)

		// Plot the estimated mixture
		/*val estMixPlot: Plot = FunctionPlot(
			function = (x:Double) => estimatedMixture.p(x),
			pathRenderer = Some(PathRenderer.default(
				color = Some(HTMLNamedColors.cyan),
				label = Text(msg = "Estimated mixture"),
				lineStyle = Some(LineStyle.Dashed),
				strokeWidth = Some(2.0)
			)),
			xbounds = Some(Bounds(xMIN, xMAX)) // NOTE necessary to include x bounds or graphs WON'T appear
		)*/

		println(s"\nfrom getMixtureTrueEstimated: getXBounds => ${(xMIN, xMAX)}")

		//(canonPlot, estMixPlot)
		(canonPlot, None)
	}




	/**
	 *
	 * @param sampleData the sample data of each of the mixture components, flattened into one array
	 * @param canonParamMix the canonical mixture object (parametric) --- is made of the original distributions with
	 *                      the original parameters
	 * @param estParamMix the estimated mixture object from data (parametric) --- has estimated the original
	 *                    parameters based off data, mirroring the canonical mixture object's parameters
	 * @param kernelNonParamMix the estimated kernel density mixture object from data (non-parametric)
	 */
	def plotMixtureTrueEstimated(sampleData: Array[Double], canonicalMixture: Mixture, estimatedMixture: Mixture,
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
