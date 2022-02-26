package util.graph



// Plotting imports
import com.cibo.evilplot._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer

import com.manyangled.snowball.analysis.interpolation.MonotonicSplineInterpolator
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction

import flip.implicits._
import flip.pdf.Sketch
/**
 *
 */


object PlotHistAndSpline {

	final val SAMPLE_SIZE_FROM_SKETCH: Int = 50000 // fifty thousand



	// TODO show the smile dist fit over the sketch
	// def plotFitOverSketch

	// Show the spline from a sketch (no histogram, just simple spline)
	def getSketchSpline(sketch: Sketch[Double], splineColor: Color): Plot = {

		// Logic to create the pdf spline (from Erik Erlandson)
		val rawdata: List[Double] = sketch.samples(SAMPLE_SIZE_FROM_SKETCH)._2
		//val sketch: TDigest = TDigest.sketch(rawdata)

		val ydata: Array[Double] = (0.0 until 1.0 by 0.01).toArray :+ 1.0
		val xdata: Array[Double] = ydata.map { y => sketch.icdf(y) }
		val (xmin, xmax) = (rawdata.min, rawdata.max)
		println(xmin, xmax)
		// TODO weird error xMin must be less than xMax ...??
		//(xdata.min, xdata.max)

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


		val makePDFSpline: Double => Double = x => x match {
			case x if (x < xmin) => 0.0
			case x if (x >= xmax) => 0.0
			case x => pdfspline.value(x)
		}

		val splineplot: Plot = FunctionPlot.series(
			function = (x:Double) => makePDFSpline(x),
			name = "spline-cdf-gradient",
			color = splineColor, //HTMLNamedColors.darkMagenta,
			xbounds = Some(Bounds(xmin, xmax))
		)
		splineplot

	}


	// plot histogram from the sketch (one single one)
	def getSketchHist(sketch: Sketch[Double], histColor: Color): Plot = {

		// Create the sample data from the sketch for the histogram
		val rawData: List[Double] = sketch.samples(SAMPLE_SIZE_FROM_SKETCH)._2
		// Prepare the x-y-bounds for the graph view
		/*val ydata: Array[Double] = (0.0 until 1.0 by 0.01).toArray :+ 1.0
		val xdata: Array[Double] = ydata.map { y => sketch.icdf(y) }
		val (xmin, xmax) = (xdata.min, xdata.max)*/

		val makeHist: Seq[Double] => Plot = data => Histogram(
			data,
			//barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.blueViolet.copy(opacity = 0.25)))),
			barRenderer = Some(BarRenderer.default(color = Some(histColor.opacity(0.25)))),
			binningFunction = Histogram.density,
			xbounds = Some(Bounds(data.min, data.max)) // find the xbounds
		)

		makeHist(rawData)
	}


	def plotHistAndSpline(sketch: Sketch[Double], histSplineColor: Color): Any = {
		val histPlot: Plot = getSketchHist(sketch, histSplineColor)
		val splinePlot: Plot = getSketchSpline(sketch, histSplineColor)

		// overlay (combine the plots)
		val overlayPlot = Overlay(histPlot, splinePlot)
			.xAxis()
			.yAxis()
			.standard() //.frame()
			.xLabel("x")
			.yLabel("y")
			/*.overlayLegend(x = 0.8)*/.render()

		displayPlot(overlayPlot)
	}


	def plotMovingHistsWithSpline(sketches: Seq[Sketch[Double]], HOW_MANY: Int = 5): Any = {
		// Create indexed list of sketches
		val indexedSketches: Seq[(Int, Sketch[Double])] = sketches.indices.zip(sketches)

		// Select just few for plotting (max 5 for now)
		val howManyToShow: Int = HOW_MANY
		val step: Int = scala.math.ceil(indexedSketches.length * 1.0 / howManyToShow).toInt
		val shorterIndexedSketches = indexedSketches.filter{ case (idx, _) => idx % step == 0}

		val colorSeq: Seq[Color] = Color.getGradientSeq(shorterIndexedSketches.length)

		// Get samples each sketch in order to create the splines / hists
		val sketchesWithPlots: Seq[(Int, Sketch[Double], Plot, Plot)] = shorterIndexedSketches
			.zip(colorSeq)
			.drop(1) // to avoid the xmin not < xmax error
			.map{ case ((idx, skt), color) => (idx, skt, getSketchHist(skt, color), getSketchSpline(skt, color))}

		val allPlots: Seq[Plot] = sketchesWithPlots.flatMap{ case(_, _, hist, spline) => List(hist, spline) }

		val plt: Drawable = Overlay(allPlots:_*) //Overlay(h, s)
			.xAxis()
			.yAxis()
			.standard() //.frame()
			.xLabel("x")
			.yLabel("y").render()
		/*.overlayLegend(x=0.8).*/

		displayPlot(plt)
	}

}