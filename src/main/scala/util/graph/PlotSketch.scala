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

import flip.implicits._
import flip.pdf.Sketch

import smile.stat.distribution.{KernelDensity, Mixture}

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._

import util.graph.PlotDensity._

import scala.reflect.runtime.universe._



/**
 * These functinos are related to plotting involving Flip library's Sketch object
 */

object PlotSketchData {

	final val SAMPLE_SIZE: Int = 8000 //50000 // fifty thousand

}
import PlotSketchData._

object PlotSketch {


	// Show the spline from a sketch (no histogram, just simple spline)
	def getSketchSpline(sketch: Sketch[Double], splineColor: Color,
					dotted: Boolean = false,
					label: Option[String] = None): Plot = {

		// Logic to create the pdf spline (from Erik Erlandson)
		val sampleData: List[Double] = sketch.samples(SAMPLE_SIZE)._2

		val ydata: Array[Double] = (0.0 until 1.0 by 0.01).toArray :+ 1.0
		val xdata: Array[Double] = ydata.map { y => sketch.icdf(y) }
		val (xmin, xmax) = (sampleData.min, sampleData.max)
		println(s"From getSketchSpline: printing (xmin, xmax) = ($xmin, $xmax)")

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

		val splineplot: Plot = dotted match {

			case true => FunctionPlot(
				function = (x:Double) => makePDFSpline(x),
				pathRenderer = Some(PathRenderer.default(
					color = Some(splineColor),
					label = Text(msg = label.getOrElse("NO LABEL (spline)")),
					strokeWidth = Some(3.0),
					lineStyle = if(dotted) Some(LineStyle.Dashed) else None //if(dotted) Some(LineStyle(dashPattern = Seq(2.0))) else None )
				)),
				xbounds = Some(Bounds(xmin, xmax))
			)
			case false => FunctionPlot.series(
				function = (x:Double) => makePDFSpline(x),
				xbounds = Some(Bounds(xmin, xmax)),
				name = label.getOrElse("NO LABEL (spline)"),
				color = splineColor //HTMLNamedColors.darkMagenta,
			)
		}

		splineplot
	}

	def getSpline(sampleData: Seq[Double], splineColor: Color,
			    dotted: Boolean = false,
			    label: Option[String] = None): Plot	= {

		// NOTE: need to generate a sketch here in order to calculaet from the inverse cdf, the xdata
		val emptySketch = Sketch.empty[Double]
		val sketch = sampleData.foldLeft(emptySketch) {
			case (sketch, sampleValue) => sketch.update(sampleValue)
		}

		getSketchSpline(sketch, splineColor, dotted, label)
	}

	def getHist(data: Seq[Double], histColor: Color, label: Option[String] = None): Plot = {

		val makeHist: Seq[Double] => Plot = data => Histogram(
			data,
			//barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.blueViolet.copy(opacity = 0.25)))),
			name = label, // TODO why doesn't this how up?
			barRenderer = Some(BarRenderer.default(color = Some(histColor.opacity(0.25)))),
			binningFunction = Histogram.density,
			xbounds = Some(Bounds(data.min, data.max)) // find the xbounds
		)

		makeHist(data)
	}

	// plot histogram from the sketch (one single one)
	def getSketchHist(sketch: Sketch[Double], histColor: Color, label: Option[String] = None): Plot = {

		// Create the sample data from the sketch for the histogram
		val rawData: List[Double] = sketch.samples(SAMPLE_SIZE)._2

		getHist(rawData, histColor, label)
	}


	/*def plotHistSplineFromOneSketch(sketch: Sketch[Double],
							  histSplineColor: Color,
							  titleName: Option[String] = None,
							  dotted: Boolean = false,
							  label: Option[String] = None): Any = {

		val histPlot: Plot = getSketchHist(sketch, histSplineColor, label) // prefer label on hist, since can be
		// many lines similar to spline
		val splinePlot: Plot = getSketchSpline(sketch, histSplineColor, dotted)

		// overlay (combine the plots)
		val overlayPlot = Overlay(histPlot, splinePlot)
			.xAxis()
			.yAxis()
			.title(titleName.getOrElse(""))
			.standard() //.frame()
			.overlayLegend() // for name labels to appear
			.xLabel("x")
			.yLabel("y")
			.render()

		displayPlot(overlayPlot)
	}*/

	def plotHistSplineFromTimeData(timeData: Seq[(Int, Seq[Double])],
							 titleName: Option[String] = None,
							 HOW_MANY: Option[Int] = Some(10),
							 dotted: Boolean = false,
							 label: Option[String] = None): Any = {

		// Select just few for plotting (max 5 for now)
		val howManyToShow: Int = HOW_MANY.isDefined match {
			case false => timeData.length
			case true => timeData.length < HOW_MANY.get match {
				case true => timeData.length
				case false => HOW_MANY.get
			}
		}
		val step: Int = scala.math.ceil(timeData.length * 1.0 / howManyToShow).toInt
		val shorterIndexedSamples = timeData.filter{ case (idx, _) => idx % step == 0}

		//val colorSeq: Seq[Color] = Color.getGradientSeq(shorterIndexedSamples.length)

		val BLACK = HTMLNamedColors.black

		// Get samples each sketch in order to create the splines / hists
		val samplesWithPlots: Seq[(Int, Seq[Double], Plot, Plot)] = shorterIndexedSamples
			.drop(1) // to avoid the xmin not < xmax error
			.map{ case (idx, samp) =>
				(idx, samp, getHist(samp, BLACK, label), getSpline(samp, BLACK, dotted))
			}
		/*val samplesWithPlots: Seq[(Int, Seq[Double], Plot, Plot)] = shorterIndexedSamples
			.zip(colorSeq)
			.drop(1) // to avoid the xmin not < xmax error
			.map{ case ((idx, samp), color) => (idx, samp, getHist(samp, color), getSpline(samp, color))}*/

		val allPlots: Seq[Plot] = samplesWithPlots.flatMap{ case(_, _, hist, spline) => List(hist, spline) }

		val plt: Drawable = Overlay(allPlots:_*) //Overlay(h, s)
			.xAxis()
			.yAxis()
			.title(titleName.getOrElse(""))
			.standard() //.frame()
			.overlayLegend() // for name labels to appear
			.xLabel("x")
			.yLabel("y").render()
		/*.overlayLegend(x=0.8).*/

		displayPlot(plt)
	}


	def plotHistSplineFromData(datas: Seq[Seq[Double]],
						  titleName: Option[String] = None,
						  HOW_MANY: Option[Int] = Some(10),
						  dotted: Boolean = false, //TODO move the non-option arg to be before all the other
						  // option-args
						  label: Option[String] = None): Any = {

		val indexedSamples: Seq[(Int, Seq[Double])] = datas.indices.zip(datas)

		plotHistSplineFromTimeData(indexedSamples, titleName, HOW_MANY, dotted, label)
	}






	def getSketchHistSplines(sketches: Seq[Sketch[Double]],
						 HOW_MANY: Option[Int] = Some(5),
						 givenColorSeq: Option[Seq[Color]] = None,
						 graphToColorLabels: Option[Seq[String]] = None): Seq[Plot] = {

		// Create indexed list of sketches
		val indexedSketches: Seq[(Int, Sketch[Double])] = sketches.indices.zip(sketches)

		// Select just few for plotting (max 5 for now)
		val howManyToShow: Int = HOW_MANY.isDefined match {
			case false => indexedSketches.length
			case true => indexedSketches.length <= HOW_MANY.get match {
				case true => indexedSketches.length
				case false => HOW_MANY.get
			}
		}

		val step: Int = scala.math.ceil(indexedSketches.length * 1.0 / howManyToShow).toInt
		val shorterIndexedSketches = indexedSketches.filter{ case (idx, _) => idx % step == 0}

		println(s"step = $step, lengthshortersketches.length = ${shorterIndexedSketches.length}")

		val colorSeq: Seq[Color] = givenColorSeq.isDefined match {
			case false => Color.getGradientSeq(shorterIndexedSketches.length)
			case true => givenColorSeq.get
		}

		assert(colorSeq.length == howManyToShow, "ERROR: lengths of colors must equal length of sketches and " +
			"number to show")
		assert(howManyToShow == shorterIndexedSketches.length,
			"ERROR: length of colors must equal length of sketches")

		// Flag whether to use dotted line for splines or not
		val isDotted: Boolean = true // if(originalDists.isDefined) true else false

		// Get samples each sketch in order to create the splines / hists
		val sketchesWithPlots: Seq[(Int, Sketch[Double], Plot, Plot)] = graphToColorLabels.isDefined match {
			case true => shorterIndexedSketches
				.zip(colorSeq)
				.zip(graphToColorLabels.get)
				//.drop(1) // to avoid the xmin not < xmax error NOTE have to do this BEFORE passing function arg
				.map{ case (((idx, skt), color), label) =>
					//(idx, skt, getSketchHist(skt, color, Some(label)), getSketchSpline(skt, color, dotted))
					// TODO figure out how to get label from the histogram ???
					(idx, skt, getSketchHist(skt, color), getSketchSpline(skt, color, isDotted, Some(label)))
				}
			case false => shorterIndexedSketches.zip(colorSeq)
				//.drop(1) // to avoid the xmin not < xmax error NOTE have to do this BEFORE passing function arg
				.map{ case ((idx, skt), color) =>
					(idx, skt, getSketchHist(skt, color), getSketchSpline(skt, color, isDotted))
				}
		}

		val histsAndSplines: Seq[Plot] = sketchesWithPlots.flatMap{ case(_, _, hist, spline) => List(hist, spline) }


		return histsAndSplines
	}

	// TODO do fold starting with overlay of splines and legend then .overlay of each hist thereafter
	def plotSketchHistSplines[T: Numeric, D](sketches: Seq[Sketch[Double]],
						 originalDists: Option[Seq[Distr[T, D]]] = None,
						titleName: Option[String] = None,
						HOW_MANY: Option[Int] = Some(5),
						givenColorSeq: Option[Seq[Color]] = None,
						graphToColorLabels: Option[Seq[String]] = None,
						 overlayMixture: Boolean = false)(implicit evSamp: Sampling[T, Distr[T, D]]): Unit = {

		// Get xbounds for the plot
		val sampleData: Seq[Double] = sketches.flatMap(_.samples(SAMPLE_SIZE)._2)
		val (xMIN, xMAX): (Double, Double) = (sampleData.min, sampleData.max)

		val histsAndSplines: Seq[Plot] = getSketchHistSplines(sketches, HOW_MANY, givenColorSeq,
			graphToColorLabels)

		import util.graph.PlotMixture._

		val mixtures: List[Plot] = overlayMixture && originalDists.isDefined match {
			case true => {
				val (canonPlot, estPlot) = getMixtureTrueEstimated(sketches.last, originalDists.get)
				List(canonPlot, estPlot)
			}
			case false => List[Plot]()
		}

		val plt: Drawable = Overlay((histsAndSplines ++ mixtures): _*)
			.xAxis()
			.yAxis()
			.xbounds(lower = xMIN, upper = xMAX)
			.title(titleName.getOrElse(""))
			.standard() //.frame()
			.overlayLegend() // for name labels to appear
			.xLabel("x")
			.yLabel("y").render() //.frame().render()
		/*.overlayLegend(x=0.8).*/

		displayPlot(plt)
	}

	// TODO make a general version contaiining the boilerplate code common toboth, returning just the plot object
	//  before all the drawable and display business, and let that get returned in THIS ONE with original dists arg,
	//  so you can have the original dists logic separately, and combine the two overlayplots separately
	def getSketchHistSplineWithDists[T: TypeTag : Numeric, D](sketches: Seq[Sketch[Double]],
							 HOW_MANY: Option[Int] = Some(5),
							 givenColorSeq: Option[Seq[Color]] = None,
							 graphToColorLabels: Option[Seq[String]] = None,
							 originalDists: Seq[Distr[T, D]])
												  (implicit evProb: ProbabilityFunction[T, D],
												   evSamp: Sampling[T, Distr[T, D]])
	: Seq[Plot]= {

		// Get first part of the plotting
		val histsAndSplines: Seq[Plot] = getSketchHistSplines(sketches, HOW_MANY, givenColorSeq,
			graphToColorLabels)

		// Create the original dist objects
		// Select just few for plotting (max 5 for now)
		val howManyToShow: Int = histsAndSplines.length

		val step: Int = scala.math.ceil(originalDists.length * 1.0 / howManyToShow).toInt

		val shorterOriginalDists: Seq[(Int, Distr[T, D])] = originalDists.indices
			.zip(originalDists)
			.filter{ case (idx, _) => idx % step == 0}

		val colorSeq: Seq[Color] = givenColorSeq.isDefined match {
			case false => Color.getGradientSeq(shorterOriginalDists.length)
			case true => givenColorSeq.get
		}

		val densities: Seq[Plot] = shorterOriginalDists.unzip._2
			.zip(colorSeq)
			.map{ case (dst, color) => getDensity(dst, color)}

		/*val plots: Seq[(Plot, Plot)] = sketchesWithPlots.map{ case (_, _, hist, spline) => (hist, spline )}
		val (hists, splines): (Seq[Plot], Seq[Plot]) = (plots.unzip._1, plots.unzip._2)*/
		histsAndSplines ++ densities
	}

	def plotSketchHistSplineWithDists[T: TypeTag : Numeric, D](sketches: Seq[Sketch[Double]],
												   titleName: Option[String] = None,
												   HOW_MANY: Option[Int] = Some(5),
												   givenColorSeq: Option[Seq[Color]] = None,
												   graphToColorLabels: Option[Seq[String]] = None,
												   originalDists: Seq[Distr[T, D]])
												   (implicit evProb: ProbabilityFunction[T, D],
												    evSamp: Sampling[T, Distr[T, D]]): Unit = {

		// Get xbounds for the plot
		//val sampleData: Seq[Double] = sketches.flatMap(_.samples(SAMPLE_SIZE)._2)
		val sampleDistData: Seq[Double] = originalDists.flatMap(dst => dst.sample(SAMPLE_SIZE))
			.map(t => implicitly[Numeric[T]].toDouble(t))

		val (xMIN, xMAX): (Double, Double) = (sampleDistData.min, sampleDistData.max)

		val histSplineDensityPlots: Seq[Plot] = getSketchHistSplineWithDists(sketches, HOW_MANY, givenColorSeq,
			graphToColorLabels, originalDists)

		// TODO do fold starting with overlay of splines and legend then .overlay of each hist thereafter
		val plt: Drawable = Overlay(histSplineDensityPlots:_*)
			/* Overlay(splines:_*).topLegend(labels = graphToColorLabels)
			.overlay(hists:_*)*/
			.xAxis()
			.yAxis()
			.xbounds(lower = xMIN, upper = xMAX)
			.title(titleName.getOrElse(""))
			.standard() //.frame()
			.overlayLegend() // for name labels to appear
			.xLabel("x")
			.yLabel("y").render()//.frame().render()
		/*.overlayLegend(x=0.8).*/

		displayPlot(plt)
	}


}