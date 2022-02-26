package Project_TSketches.FlipLibrary


import flip.implicits._
import flip.pdf.Sketch

import com.cibo.evilplot._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.numeric._ // Point
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer



//import org.isarnproject.sketches.TDigest

/**
 *
 */

import util.graph.PlotHistAndSpline._




object Example_IncrementalConceptDrift extends App {


	val expName = "incremental-cd-normal"
	val dataNo = 10000 // TODO DEBUG next reduce sample_size_from_sketch and increasea this one to 50,000
	// changed dataNo (was 1000)
	val draftStart = 300
	val draftStartingPoint = 0.0
	val velocity = 0.01

	/**
	 * Concept drift component #1 =
	 * -- Passes a center value and then if past a certain point, reverts back to  earlier values (cycling through
	 * means so that the distributions add up cyclically (?). Otherwise, makes the center as function of velocity
	 * and distance from a given point (draftStart)
 	 */
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



	//val idxPdf = idxSketches.map { case (idx, sketch) => (idx, sketch.barPlot.csv) }
	//val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }


	// TODO plot these measures as boxplots

	// TODO some to get total of 50 (1000/50 = 20 so every 20th one) -- to see how this measure goes down as sketch
	//  count increases (diff between udnerlying and sketch should get smaller)
	val idxDel = idxSketches.map { case (idx, sketch) => (idx, Delta(underlying(idx), sketch).csv) }
	// TODO plot kl divergences
	val idxKld = idxSketches.map { case (idx, sketch) => (idx, KLD(underlying(idx), sketch)) }
	val idxKldNaNFree = idxKld.filter {case (idx, measure) => ! measure.isNaN}

	displayPlot(ScatterPlot(
		data = idxKldNaNFree.map{ case (idx, kld) => Point(idx * 1.0, kld) }
	)
		.standard()
		.xLabel("x")
		.yLabel("y")
		.title("KL Divergence at each Index (Comparing Sketch to Underlying Distribution)")
		.render()
	)

	// TODO plot
	val idxCos = idxSketches.map { case (idx, sketch) => (idx, Cosine(underlying(idx), sketch)) }
	val idxCosNaNFree = idxCos.filter {case (idx, measure) => ! measure.isNaN}

	displayPlot(ScatterPlot(
		data = idxCosNaNFree.map{ case (idx, cos) => Point(idx * 1.0, cos) }
	)
		.standard()
		.xLabel("x")
		.yLabel("y")
		.title("Cosine Distance at each Index (Comparing Sketch to Underlying Distribution)")
		.render()
	)

	// TODO plot
	val idxEuc = idxSketches.map { case (idx, sketch) => (idx, Euclidean(underlying(idx), sketch)) }
	val idxEucNaNFree = idxEuc.filter {case (idx, measure) => ! measure.isNaN}

	displayPlot(ScatterPlot(
		data = idxEucNaNFree.map{ case (idx, euc) => Point(idx * 1.0, euc) }
	)
		.standard()
		.xLabel("x")
		.yLabel("y")
		.title("Euclidean Distance at each Index (Comparing Sketch to Underlying Distribution)")
		.render()
	)

	val idxED = idxSketches.map { case (idx, sketch) => (idx, ED(underlying(idx), sketch)) }
	val idxEDNaNFree = idxED.filter {case (idx, measure) => ! measure.isNaN}

	displayPlot(ScatterPlot(
		data = idxEDNaNFree.map{ case (idx, ed) => Point(idx * 1.0, ed) }
	)
		.standard()
		.xLabel("x")
		.yLabel("y")
		.title("ED at each Index (Comparing Sketch to Underlying Distribution)")
		.render()
	)

	val idxMedian = idxSketches.map { case (idx, sketch) => (idx, sketch.median) }
	val idxMedianNaNFree = idxMedian.filter {case (idx, measure) => ! measure.isNaN}

	displayPlot(ScatterPlot(
		data = idxMedianNaNFree.map{ case (idx, medianPoint) => Point(idx * 1.0, medianPoint) }
	)
		.standard()
		.xLabel("x")
		.yLabel("y")
		.title("Median at each Index (Comparing Sketch to Underlying Distribution)")
		.render()
	)

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
		s" Memory usage (byte): $mem"
	println(str)*/

}
