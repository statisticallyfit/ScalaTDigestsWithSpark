package Project_TSketches.FlipLibrary

import flip.implicits.{Cosine, Delta, ED, Euclidean, KLD, NumericDist, Sketch, SketchConf}

import util.graph.PlotSketch._

/**
 * A experiment for sudden concept drift.
 * https://edouardfouche.com/img/concept-drift/conceptdrift.png
 * */
object Example_SuddenConceptDrift extends App {

	val expName = "sudden-cd-normal"
	val dataNo = 10000 // was just 1000
	val draftStart = 300

	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-10),
		cmapEnd = Some(10)
	)
	val sketch0 = Sketch.empty[Double]
	val (mean1, mean2) = (0.0, 5.0)

	/**
	 * Sudden concept drift ponent #1:
	 * Suddenly changes the index (mean)
	 */
	def center(idx: Int) = if (idx < draftStart) mean1 else mean2

	// note: variance was 1.0, changed by me to 10.0
	def underlying(idx: Int) = NumericDist.normal(center(idx), 10.0, idx)

	val normalDists = (1 to dataNo).map(idx => underlying(idx))
	val datas = normalDists.map(_.sample._2).toList
	val sketchTraces = (sketch0 :: sketch0.updateTrace(datas)).drop(1)

	val normalDistsEveryTenth = normalDists.indices.zip(normalDists)
		.filter {case (idx, _) => idx % 10 == 0}
		.drop(1) // drop 1 to match the length of the sketches (not including the empty sketch)
	val normalOneSketchEveryTenth = sketchTraces.indices.zip(sketchTraces)
		.filter { case (idx, _) => idx % 10 == 0 }
		.unzip._2
	//val idxPdf = idxSketches.map { case (idx, sketch) => (idx, sketch.barPlot.csv) }
	//val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }


	// TODO graph the moving hists with splines
	println(s"datas.length = ${datas.length}")
	println(s"normalOneSketchEveryTenth.length = ${normalOneSketchEveryTenth.length}")

	/*plotSketchHistSplines(normalOneSketchEveryTenth, // drop the empty sketch at beginning
		titleName = Some(s"One-single Sample: Normal sketches Using Flip Center Drift (including draftStart)"),
		//givenColorSeq = Some(List(HTMLNamedColors.blue)),
		graphToColorLabels = Some(normalDistsEveryTenth.drop(draftStart).map(_.toString)),
		originalDists = Some(normalDistsEveryTenth.drop(draftStart)),
		overlayMixture = true
	)*/


	// TODO graph these measures as boxplots
	/*val idxDel = normalOneSketchEveryTenth.map { case (idx, sketch) => (idx, Delta(underlying(idx), sketch).csv) }
	val idxKld = normalOneSketchEveryTenth.map { case (idx, sketch) => (idx, KLD(underlying(idx), sketch)) }
	val idxCos = normalOneSketchEveryTenth.map { case (idx, sketch) => (idx, Cosine(underlying(idx), sketch)) }
	val idxEuc = normalOneSketchEveryTenth.map { case (idx, sketch) => (idx, Euclidean(underlying(idx), sketch)) }
	val idxED = normalOneSketchEveryTenth.map { case (idx, sketch) => (idx, ED(underlying(idx), sketch)) }
	val idxMedian = normalOneSketchEveryTenth.map { case (idx, sketch) => (idx, sketch.median) }*/

/*	ExpOutOps.clear(expName)
	ExpOutOps.writeStrs(expName, "pdf", idxPdf)
	ExpOutOps.writeStrs(expName, "cdf", idxCdf)
	ExpOutOps.writeStrs(expName, "delta", idxDel)
	ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
	ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
	ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
	ExpOutOps.writeStr(expName, "ed", idxED.map { case (idx, ed) => s"$idx, $ed" }.mkString("\n"))
	ExpOutOps.writeStr(
		expName,
		"median",
		idxMedian.map { case (idx, sktMed) => s"$idx, ${center(idx)}, $sktMed" }.mkString("\n"))*/


}
