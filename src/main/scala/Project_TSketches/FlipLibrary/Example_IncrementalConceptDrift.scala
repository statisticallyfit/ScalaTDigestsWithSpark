//package Project_TSketches.FlipLibrary
//
//
//import flip.implicits._
//import flip.pdf.Sketch
//import util.TMeasure._
//
//import util.graph.PlotSketch._
//import util.graph.PlotDensity._
//
//import scala.language.implicitConversions
//
//import util.EnhanceFlipSketchUpdate._
//
//
//import util.distributionExtensions.distributions._
//import util.distributionExtensions.instances.AllInstances._
//import util.distributionExtensions.syntax._
//
////import org.isarnproject.sketches.TDigest
//
///**
// *
// */
//
//
//object Example_IncrementalConceptDrift extends App {
//
//
//	val expName = "incremental-cd-normal"
//	val dataNo = 10000 // TODO DEBUG next reduce sample_size_from_sketch and increasea this one to 50,000
//	// changed dataNo (was 1000)
//	val draftStart = 300
//	val draftStartingPoint = 0.0
//	val velocity = 0.01
//
//
//	val SAMPLE_SIZE = 1000
//
//	/**
//	 * Concept drift component #1 =
//	 * -- Passes a center value and then if past a certain point, reverts back to  earlier values (cycling through
//	 * means so that the distributions add up cyclically (?). Otherwise, makes the center as function of velocity
//	 * and distance from a given point (draftStart)
// 	 */
//	def center(idx: Int): Double =
//		if (draftStart > idx) draftStartingPoint
//		else draftStartingPoint + velocity * (idx - draftStart)
//
//	def underlying(idx: Int): NumericDist[Double] = NumericDist.normal(center(idx), 10.0, idx)
//
//	val normalDists: List[NumericDist[Double]] = (0 to dataNo).toList.map(idxTime => underlying(idxTime))
//	val normalDistsEveryTenth: List[NumericDist[Double]] = normalDists.drop(draftStart).indices.zip(normalDists.drop(draftStart))
//		.filter{ case (idx, _) => idx	% 10 == 0}.unzip._2.toList
//
//	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
//	// the dist
//	val normalOneSampleData: List[Double] = normalDists.map(_.sample._2)
//	val normalMultiSampleData: List[List[Double]] = normalDists.map(_.samples(SAMPLE_SIZE)._2.toList).toList
//
//
//	// TODO ATTEMPT: graphing the time  - with - dist-sketch ---------------------------------------------------------------
//	//val distTimePairs: Seq[(Int, List[Double])] = normalMultiSampleData.indices.zip(normalMultiSampleData)
//	//plotMovingTimeDataWithSpline(distTimePairs, HOW_MANY = 20)
//	// ----------------------------------------------------------------------------------------------------------------
//
//	implicit val conf: SketchConf = SketchConf(
//		cmapStart = Some(-40.0),
//		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
//	)
//
//	val sketch0: Sketch[Double] = Sketch.empty[Double]
//	val sketch00: Sketch[Double] = Sketch.empty[Double]
//
//
//
//
//	// Creating the sketches and combining them:
//	// TESTING what happens when sketch is made from sample(1)
//	val normalOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(normalOneSampleData))
//		.drop(draftStart) // drop the ones with  mean == 0
//	val normalOneEveryTenthSketches = normalOneSampleSketches.indices.zip(normalOneSampleSketches)
//		.filter{ case(idx, _) => idx % 10 == 0}
//		.unzip._2
//
//	/*plotSketchHistSplines(timeSketchesFromOneDataEveryTenth.unzip._2.drop(1),
//		titleName = Some("Sketches from sample size = 1"))*/
//	// TODO error requires to separate the original dists func from the non-original dists func to avoid the
//	//  typeerror when not passing the original dists func (separate FUNCTIONS)
//
//
//	// TESTING: what happens when sketch is made from a larger sample than just 1
//	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
//	val normalMultiSampleSketches: Seq[Sketch[Double]] = (sketch00 :: sketch00.updateWithMany(normalMultiSampleData))
//		.drop(draftStart) // drop the ones with mean == 0
//	val normalMultiEveryTenthSketches = normalMultiSampleSketches.indices.zip(normalMultiSampleSketches)
//		.filter{ case(idx, _) => idx % 10 == 0}
//		.unzip._2
//
//
//	// PLOTTING
//	println(s"normalDistsEveryTenth.length = ${normalDistsEveryTenth.length}")
//	println(s"normalOneEveryTenthSketches.length = ${normalOneEveryTenthSketches.length}")
//	println(s"normalMultiEveryTenthSketches.length = ${normalMultiEveryTenthSketches.length}")
//
//
//
//	// Just changing the flip objects to my object so that can pass to my plotting function
//	val flipNormalDists: List[flip.pdf.NormalDist[Double]] = normalDistsEveryTenth.map(nn => nn.asInstanceOf[flip.pdf.NormalDist[Double]])
//	val myNormalDists: List[NormalDist] = flipNormalDists.map(nn => NormalDist(mu = nn.mean, std = scala.math.sqrt(nn.variance)))
//
//	plotDensities(myNormalDists, HOW_MANY = Some(10))
//
//
//	plotSketchHistSplines(normalOneEveryTenthSketches, //.drop(1), // drop the empty sketch at beginning
//		titleName = Some(s"One-single Sample: (Flip's) Normal sketches Using Flip Center Drift (left out first " +
//			s"`draftNum` sketches)"),
//		//givenColorSeq = Some(List(HTMLNamedColors.blue)),
//		graphToColorLabels = Some(myNormalDists.drop(draftStart).map(_.toString)),
//		originalDists = myNormalDists.drop(draftStart),
//		overlayMixture = true
//	)
//
//
//	plotSketchHistSplines(normalMultiEveryTenthSketches, //.drop(1), // drop the empty sketch at beginning
//		titleName = Some(s"Multi-batch samples: (Fli's) Normal Sketches, Sample size = $SAMPLE_SIZE (left out first" +
//			s" `draftNum` sketches)"),
//		graphToColorLabels = Some(myNormalDists.drop(draftStart).map(_.toString)),
//		originalDists = myNormalDists.drop(draftStart),
//		overlayMixture = true
//	)
//
//
//
//	// NOTE: Indeed the x-axis represents time because the `updateTrace` from Flip returns one sketch per sampled
//	//  value that corresponds to the time (index) it was mapped to, so then you can say that sketch corresponds to that
//	//  time  (index).
//
//	/*val timeSketchesFromMultiDataEveryTenth: List[(Int, Sketch[Double])] =
//		normalMultiEveryTenthSketches.indices.zip(normalMultiEveryTenthSketches).toList
//
//	// NOTE: drop 1 to avoid the xMin < xMax 'not' error
//	plotSketchHistSplines(timeSketchesFromMultiDataEveryTenth.unzip._2.drop(1),
//		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"))*/
//
//
//
//	//val idxPdf = idxSketches.map { case (idx, sketch) => (idx, sketch.barPlot.csv) }
//	//val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }
//
//
//
//	/*val idxDel = idxSketches.map { case (idx, sketch) => (idx, Delta(underlying(idx), sketch).csv) }
//
//	val idxKld = idxSketches.map { case (idx, sketch) => (idx, KLD(underlying(idx), sketch)) }
//	val idxKldNaNFree = idxKld.filter {case (idx, measure) => ! measure.isNaN}
//
//	displayPlot(ScatterPlot(
//		data = idxKldNaNFree.map{ case (idx, kld) => Point(idx * 1.0, kld) }
//	)
//		.standard()
//		.xLabel("x")
//		.yLabel("y")
//		.title("KL Divergence at each Index (Comparing Sketch to Underlying Distribution)")
//		.render()
//	)
//
//
//	val idxCos = idxSketches.map { case (idx, sketch) => (idx, Cosine(underlying(idx), sketch)) }
//	val idxCosNaNFree = idxCos.filter {case (idx, measure) => ! measure.isNaN}
//
//	displayPlot(ScatterPlot(
//		data = idxCosNaNFree.map{ case (idx, cos) => Point(idx * 1.0, cos) }
//	)
//		.standard()
//		.xLabel("x")
//		.yLabel("y")
//		.title("Cosine Distance at each Index (Comparing Sketch to Underlying Distribution)")
//		.render()
//	)
//
//
//	val idxEuc = idxSketches.map { case (idx, sketch) => (idx, Euclidean(underlying(idx), sketch)) }
//	val idxEucNaNFree = idxEuc.filter {case (idx, measure) => ! measure.isNaN}
//
//	displayPlot(ScatterPlot(
//		data = idxEucNaNFree.map{ case (idx, euc) => Point(idx * 1.0, euc) }
//	)
//		.standard()
//		.xLabel("x")
//		.yLabel("y")
//		.title("Euclidean Distance at each Index (Comparing Sketch to Underlying Distribution)")
//		.render()
//	)
//
//	val idxED = idxSketches.map { case (idx, sketch) => (idx, ED(underlying(idx), sketch)) }
//	val idxEDNaNFree = idxED.filter {case (idx, measure) => ! measure.isNaN}
//
//	displayPlot(ScatterPlot(
//		data = idxEDNaNFree.map{ case (idx, ed) => Point(idx * 1.0, ed) }
//	)
//		.standard()
//		.xLabel("x")
//		.yLabel("y")
//		.title("ED at each Index (Comparing Sketch to Underlying Distribution)")
//		.render()
//	)
//
//	val idxMedian = idxSketches.map { case (idx, sketch) => (idx, sketch.median) }
//	val idxMedianNaNFree = idxMedian.filter {case (idx, measure) => ! measure.isNaN}
//
//	displayPlot(ScatterPlot(
//		data = idxMedianNaNFree.map{ case (idx, medianPoint) => Point(idx * 1.0, medianPoint) }
//	)
//		.standard()
//		.xLabel("x")
//		.yLabel("y")
//		.title("Median at each Index (Comparing Sketch to Underlying Distribution)")
//		.render()
//	)
//
//	// console print
//	val avgSize = 10
//	val avgKld = idxKld.takeRight(avgSize).map(_._2).sum / avgSize
//	val avgCos = idxCos.takeRight(avgSize).map(_._2).sum / avgSize
//	val avgEuc = idxEuc.takeRight(avgSize).map(_._2).sum / avgSize
//	//val mem = flip.Profiler.serializedMem(idxSketches.last._2)
//
//	val str = s"Similarity for incremental concept-drifted data stream with velocity $velocity: \n" +
//		s" KLD: $avgKld \n" +
//		s" Cosine: $avgCos \n" +
//		s" Euclidean: $avgEuc \n"*/
//
//}
