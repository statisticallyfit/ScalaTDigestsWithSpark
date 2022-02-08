package temp



// Plotting imports
import com.cibo.evilplot._
import com.cibo.evilplot.colors.HTMLNamedColors
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer


import flip.implicits._
import flip.pdf.Sketch
//import org.isarnproject.sketches.TDigest

/**
 *
 */


object PlotMovingDists {
	def plotDistsFromSketches(pairs: Seq[(Sketch[Double], Seq[Double])]): Any = {

		// Make the histogram objects
		val makeHist: Seq[Double] => Plot = data => Histogram(
			data,
			barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.green.copy(opacity = 0.25)))),
			binningFunction = Histogram.density,
			xbounds = Some(Bounds(data.min, data.max)) // find the xbounds
		)


		val theHists: Seq[Plot] = pairs.map{ case (sketch, dataFromSketch) => makeHist(dataFromSketch)}

		// NOTE:  combining plots here will also combine the xbounds and ybounds
		val plt: Drawable = Overlay(theHists:_*)
			.xAxis()
			.yAxis()
			.frame()
			.xLabel("x")
			.yLabel("y")
			.overlayLegend(x=0.8).render()

		///publish.png(plt.asBufferedImage)
		displayPlot(plt)
	}
}
import PlotMovingDists._

object try_FlipConceptDrift extends App {


	val expName = "incremental-cd-normal"
	val dataNo = 1000
	val draftStart = 300
	val draftStartingPoint = 0.0
	val velocity = 0.01

	def center(idx: Int) =
		if (draftStart > idx) draftStartingPoint
		else draftStartingPoint + velocity * (idx - draftStart)
	def underlying(idx: Int): NumericDist[Double] = NumericDist.normal(center(idx), 1.0, idx)
	val datas: List[Double] = (0 to dataNo).toList.map(idx => underlying(idx).sample._2)

	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-20.0),
		cmapEnd = Some(20.0)
	)
	val sketch0 = Sketch.empty[Double]
	val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
	val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }

	// TODO: histogram-plot every 250th one because there are 1000 sketches so just to see 4 of them (moving right)
	val idxPdf = idxSketches.map { case (idx, skt) => (idx, skt.barPlot.csv) }
	val idxPdf2: List[(Int, (Sketch[Double], List[Double]))] = idxSketches.map { case (idx, skt) => (idx, skt.samples(10000)) }





	val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }

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
	plotDistsFromSketches(idxPdf2.filter{case (idx, _) => idx % 250 == 0}.unzip._2)
	/*ExpOutOps.writeStrs(expName, "cdf", idxCdf)
	ExpOutOps.writeStrs(expName, "delta", idxDel)
	ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
	ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
	ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
	ExpOutOps.writeStr(expName, "ed", idxED.map { case (idx, ed) => s"$idx, $ed" }.mkString("\n"))
	ExpOutOps.writeStr(
		expName,
		"median",
		idxMedian.map { case (idx, sktMed) => s"$idx, ${center(idx)}, $sktMed" }.mkString("\n"))*/

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
	println(str)

}
