package workspace_snippets



// Plotting imports
import com.cibo.evilplot._
import com.cibo.evilplot.colors.HTMLNamedColors
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer
import com.cibo.evilplot.colors.Color

import com.manyangled.snowball.analysis.interpolation.MonotonicSplineInterpolator

import flip.implicits._
import flip.pdf.Sketch
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction


//import org.isarnproject.sketches.TDigest

/**
 *
 */


object VisualizeDists {

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

		/*displayPlot(splineplot
			.xAxis()
			.yAxis()
			.frame()
			.xLabel("x")
			.yLabel("y")
			/*.overlayLegend(x = 0.8)*/.render()
		)*/

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
		/*displayPlot(makeHist(rawdata)
			.xAxis()
			.yAxis()
			.frame()
			.xLabel("x")
			.yLabel("y")
			/*.overlayLegend(x = 0.8)*/.render()
		)*/
	}


	def plotHistAndSpline(sketch: Sketch[Double], histSplineColor: Color): Any = {
		val histPlot: Plot = getSketchHist(sketch, histSplineColor)
		val splinePlot: Plot = getSketchSpline(sketch, histSplineColor)

		// overlay (combine the plots)
		val overlayPlot = Overlay(histPlot, splinePlot)
			.xAxis()
			.yAxis()
			.frame()
			.xLabel("x")
			.yLabel("y")
			/*.overlayLegend(x = 0.8)*/.render()

		displayPlot(overlayPlot)
	}


	def plotMovingHistsWithSpline(sketches: Seq[Sketch[Double]]): Any = {
		// Create indexed list of sketches
		val indexedSketches: Seq[(Int, Sketch[Double])] = sketches.indices.zip(sketches)

		// Select just few for plotting (max 5 for now)
		val howManyToShow: Int = 5
		val step: Int = scala.math.ceil(indexedSketches.length * 1.0 / howManyToShow).toInt
		val shorterIndexedSketches = indexedSketches.filter{ case (idx, _) => idx % step == 0}


		// TODO DEBUGGING the xmin < xmax error
		/*val (h, s) = (getSketchHist(shorterIndexedSketches.last._2), getSketchSpline(shorterIndexedSketches.last._2))*/

		/*val tempselect = Seq(shorterIndexedSketches(1), shorterIndexedSketches(2), shorterIndexedSketches(3), shorterIndexedSketches(4))*/

		val colorSeq: Seq[Color] = Color.getGradientSeq(shorterIndexedSketches.length)

		// Get samples each sketch in order to create the splines / hists
		// TODO Pair up colors with hist and splines
		val sketchesWithPlots: Seq[(Int, Sketch[Double], Plot, Plot)] = shorterIndexedSketches
			.zip(colorSeq)
			.drop(1) // to avoid the xmin not < xmax error
			.map{ case ((idx, skt), color) => (idx, skt, getSketchHist(skt, color), getSketchSpline(skt, color))}


		val allPlots: Seq[Plot] = sketchesWithPlots.flatMap{ case(_, _, hist, spline) => List(hist, spline) }

		val plt: Drawable = Overlay(allPlots:_*) //Overlay(h, s)
			.xAxis()
			.yAxis()
			.frame()
			.xLabel("x")
			.yLabel("y").render()
		/*.overlayLegend(x=0.8).*/

		displayPlot(plt)
	}

}
import VisualizeDists._




object workspace_FlipLibrary_ConceptDrift extends App {


	val expName = "incremental-cd-normal"
	val dataNo = 10000 // TODO DEBUG next reduce sample_size_from_sketch and increasea this one to 50,000
	// changed dataNo (was 1000)
	val draftStart = 300
	val draftStartingPoint = 0.0
	val velocity = 0.01

	def center(idx: Int) =
		if (draftStart > idx) draftStartingPoint
		else draftStartingPoint + velocity * (idx - draftStart)
	def underlying(idx: Int): NumericDist[Double] = NumericDist.normal(center(idx), 10.0, idx)
	// Creating list of samples from underlying distribution, length = dataNo = 1000
	val datas: List[Double] = (0 to dataNo).toList.map(idx => underlying(idx).sample._2)

	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // TODO changed here from (-20, 20) --- what does this do?
	)
	val sketch0 = Sketch.empty[Double]
	val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
	val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 } // so dataNo (1000) / 10 = 100 elements left


	// NOTE: print the data length
	println(datas.length)
	println(idxSketches.length)
	plotMovingHistsWithSpline(idxSketches.unzip._2)


	// TODO: histogram-plot every 250th one because there are 1000 sketches so just to see 4 of them (moving right)
	// TODO 2: fit the distribution FUNCTION (using smile scala lib) and plot those usinhe Function.series plot vegas
//	val idxPdf = idxSketches.map { case (idx, skt) => (idx, skt.barPlot.csv) }
//	val idxPdf2: List[(Int, (Sketch[Double], List[Double]))] = idxSketches.map { case (idx, skt) => (idx, skt
//		.samples(5000)) }


//	println(s"samples from 11th sketch = ${idxPdf2(10)._2._2}")
//	//plotDistsFromSketches(Seq(idxPdf2(10)._2))
//	println(s"samples from 91th sketch = ${idxPdf2(90)._2._2}")
//	//plotMovingDistsFromSketches(Seq(idxPdf2(90)._2))
//	getSketchSpline(sketchTraces(20))
//	getSketchHist(sketchTraces(20))
//	getSketchHist(sketchTraces(95))
//	getSketchSpline(sketchTraces(95))





	/*val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }

	// TODO some to get total of 50 (1000/50 = 20 so every 20th one) -- to see how this measure goes down as sketch
	//  count increases (diff between udnerlying and sketch should get smaller)
	val idxDel = idxSketches.map { case (idx, sketch) => (idx, Delta(underlying(idx), sketch).csv) }
	// TODO plot kl divergences
	val idxKld = idxSketches.map { case (idx, sketch) => (idx, KLD(underlying(idx), sketch)) }
	// TODO plot
	val idxCos = idxSketches.map { case (idx, sketch) => (idx, Cosine(underlying(idx), sketch)) }
	// TODO plot
	val idxEuc = idxSketches.map { case (idx, sketch) => (idx, Euclidean(underlying(idx), sketch)) }
	val idxED = idxSketches.map { case (idx, sketch) => (idx, ED(underlying(idx), sketch)) }
	val idxMedian = idxSketches.map { case (idx, sketch) => (idx, sketch.median) }

	// out

	//println("pdf", idxPdf2)
	//TODO here plotDistsFromSketches(idxPdf2.filter{case (idx, _) => idx % 250 == 0}.unzip._2)
//	ExpOutOps.writeStrs(expName, "cdf", idxCdf)
//	ExpOutOps.writeStrs(expName, "delta", idxDel)
//	ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
//	ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
//	ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
//	ExpOutOps.writeStr(expName, "ed", idxED.map { case (idx, ed) => s"$idx, $ed" }.mkString("\n"))
//	ExpOutOps.writeStr(
//		expName,
//		"median",
//		idxMedian.map { case (idx, sktMed) => s"$idx, ${center(idx)}, $sktMed" }.mkString("\n"))

	// console print
	val avgSize = 10
	val avgKld = idxKld.takeRight(avgSize).map(_._2).sum / avgSize
	val avgCos = idxCos.takeRight(avgSize).map(_._2).sum / avgSize
	val avgEuc = idxEuc.takeRight(avgSize).map(_._2).sum / avgSize
	//val mem = flip.Profiler.serializedMem(idxSketches.last._2)

	val str = s"Similarity for incremental concept-drifted data stream with velocity $velocity: \n" +
		s" KLD: $avgKld \n" +
		s" Cosine: $avgCos \n" +
		s" Euclidean: $avgEuc \n"/* +
		s" Memory usage (byte): $mem"*/
	println(str)*/

}
