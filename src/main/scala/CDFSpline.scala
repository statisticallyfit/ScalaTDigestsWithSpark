/**
 * File Source = https://github.com/erikerlandson/cdf-splining-prototype/blob/master/cdf-splining-poc.ipynb
 */

// Plotting imports
import com.cibo.evilplot._
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.colors.HTMLNamedColors
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot.renderers.BarRenderer


// Import distributions to generate sample data
import org.apache.commons.math3.distribution.GammaDistribution
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.distribution.UniformRealDistribution

// Import data sketching
import org.isarnproject.sketches.TDigest

// Apache math polynomial spline
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction

// Import spline interpolation
import com.manyangled.snowball.analysis.interpolation.MonotonicSplineInterpolator

object CDFSpline {

	def generateSpliningComparisons(dist: org.apache.commons.math3.distribution.RealDistribution) {
		val rawdata: Array[Double] = Array.fill(10000) { dist.sample()}
		val sketch: TDigest = TDigest.sketch(rawdata)
		val ydata: Array[Double] = (0.0 until 1.0 by 0.01).toArray :+ 1.0

		val xdata: Array[Double] = ydata.map { y => sketch.cdfInverse(y) }
		val (xmin, xmax) = (xdata.min, xdata.max)

		val eps: Double = 1e-9
		//println(s"""xdata= ${xdata.mkString(", ")}""")
		//println(s"""ydata= ${ydata.mkString(", ")}""")

		val interpolator: MonotonicSplineInterpolator = new MonotonicSplineInterpolator()
		// set the bounds of interpolation to the data range
		interpolator.setBounds(xmin, xmax)
		// set 20 splining intervals
		interpolator.setM(20)
		// these constraints fix cdf(xmin) to be "effectively zero" and also enforce > 0
		interpolator.addEqualityConstraint(xmin, eps)
		interpolator.addGreaterThanConstraint(xmin, 0.0)
		// these constraints fix cdf(xmax) to be "effectively one" and also enforce < 1
		interpolator.addEqualityConstraint(xmax, 1.0 - eps)
		interpolator.addLessThanConstraint(xmax, 1.0)

		// get the splines that approximate CDF and the PDF
		val cdfspline: PolynomialSplineFunction = interpolator.interpolate(xdata, ydata)
		val pdfspline: PolynomialSplineFunction = cdfspline.polynomialSplineDerivative()
		val cdffit: Double => Double = (x: Double) => x match {
			case x if (x < xmin) => 0.0
			case x if (x >= xmax) => 1.0
			case x => cdfspline.value(x)
		}
		val pdffit: Double => Double = (x: Double) => x match {
			case x if (x < xmin) => 0.0
			case x if (x >= xmax) => 0.0
			case x => pdfspline.value(x)
		}
		def showCumulative: Any = {
			val histplot = Histogram(
				rawdata,
				barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.green.copy(opacity = 0.25)))),
				binningFunction = Histogram.cumulativeDensity
			)

			val cdfplot: Plot = FunctionPlot.series(
				function = (x:Double) => sketch.cdf(x),
				name  = "cdf(x)",
				color = HTMLNamedColors.red,
				xbounds	= Some(Bounds(xmin, xmax))
			)

			val splineplot: Plot = FunctionPlot.series(
				function = (x:Double) => cdffit(x),
				name = "spline(x)",
				color = HTMLNamedColors.dodgerBlue,
				xbounds = Some(Bounds(xmin, xmax))
			)

			val plt: Drawable = Overlay(histplot, cdfplot, splineplot).overlayLegend(x=0.6).render()

			//publish.png(plt.asBufferedImage)
			displayPlot(plt)
		}

		def showPDF {
			val histplot: Plot = Histogram(
				rawdata,
				barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.green.copy(opacity = 0.25)))),
				binningFunction = Histogram.density)

			val cdfplot: Plot = FunctionPlot.series(
				function =(x:Double) => (sketch.cdf(x+0.01) - sketch.cdf(x)) / 0.01,
				name = "cdf-gradient-estimate",
				color = HTMLNamedColors.red,
				xbounds = Some(Bounds(xmin, xmax))
			)

			val splineplot: Plot = FunctionPlot.series(
				function =(x:Double) => pdffit(x),
				name = "spline-cdf-gradient",
				color = HTMLNamedColors.dodgerBlue,
				xbounds = Some(Bounds(xmin, xmax))
			)

			val plt: Drawable = Overlay(histplot, cdfplot, splineplot).overlayLegend(x=0.8).render()

			///publish.png(plt.asBufferedImage)
			displayPlot(plt)
		}
		showCumulative
		showPDF
	}
}


object CDFSplineRunner extends App {

	import CDFSpline._

	generateSpliningComparisons(new NormalDistribution())
	generateSpliningComparisons(new GammaDistribution(1.0, 1.0))
}

