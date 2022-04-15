package workspace_snippets



import com.cibo.evilplot.colors.{Color, HTMLNamedColors}

import flip.implicits._
import flip.pdf.Sketch

import util.graph.PlotSketch._
import util.graph.PlotDensity._
import util.EnhanceFlipSketchUpdate._

import scala.language.implicitConversions
import scala.language.higherKinds

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._ //otherwise cannot find implicit value for evProb, evSamp
import util.distributionExtensions.syntax._


// TEMPORARY PLOT IMPORTS

import com.cibo.evilplot._
import com.cibo.evilplot.colors.{CategoricalColoring, Color, Coloring, GradientMode, HTMLNamedColors}
import com.cibo.evilplot.geometry.{Drawable, EmptyDrawable, LineStyle, Text}
import com.cibo.evilplot.numeric.{Bounds, Point}
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.{BarRenderer, PathRenderer, PointRenderer}



/**
 *
 */
object try_FlipSketch_IncrConceptDrift_ChangingExponentials_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 1000 // was 1000 - to be like the original flip example


	val (a1, b1) = (6, 0.4)
	val (a2, b2) = (9.6, 1.2)
	val (a3, b3) = (10.2, 3.1)
	val (a4, b4) = (40.0, 5.0) // (168, 12) made this crash // good (93, 13)
	val (a5, b5) = (30.6, 9.2)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenExponential: ExponentialDist = ExponentialDist(a1)
	val redExponential: ExponentialDist = ExponentialDist(a2)
	val purpleExponential = ExponentialDist(a3)
	val orangeExponential = ExponentialDist(a4)
	val blueExponential = ExponentialDist(a5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located exponentials.
	val exponentialsIncrementalMove: Seq[ExponentialDist] = List(greenExponential, redExponential, purpleExponential, orangeExponential, blueExponential)

	// TODO Increasing amount of exponential distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / exponentialsIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val exponentialRepsInOrder: Seq[ExponentialDist] = List.fill[List[ExponentialDist]](numRepsPerDist)(exponentialsIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val exponentialRepsBatchKinds: Seq[ExponentialDist] = exponentialsIncrementalMove.map(g => List.fill[ExponentialDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val exponentialOneSampleData: List[Double] = exponentialsIncrementalMove.map(_.sample).toList
	val exponentialOneSampleData_InOrder: List[Double] = exponentialRepsInOrder.map(_.sample).toList
	val exponentialOneSampleData_Batch: List[Double] = exponentialRepsBatchKinds.map(_.sample).toList

	//val exponentialMultiSampleData: List[List[Double]] = exponentialsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val exponentialMultiSampleData: List[List[Double]] = exponentialsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch0_inorder: Sketch[Double] = Sketch.empty[Double]
	val sketch0_batch: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(exponentialDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val exponentialOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(exponentialOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val exponentialOneSampleSketches: Seq[Sketch[Double]] = sketch0.updateTrace(exponentialOneSampleData)
	val exponentialOneSampleSketches_InOrder: Seq[Sketch[Double]] = sketch0_inorder.updateTrace(exponentialOneSampleData_InOrder)
	val exponentialOneSampleSketches_Batch: Seq[Sketch[Double]] = sketch0_batch.updateTrace(exponentialOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val exponentialMultiSampleSketches: Seq[Sketch[Double]] = sketch00.updateWithMany(exponentialMultiSampleData)


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"exponentialsIncrementalMove.length = ${exponentialsIncrementalMove.length}")
	println(s"exponentialOneSampleSketches.length = ${exponentialOneSampleSketches.length}")
	println(s"exponentialOneSampleSketches_InOrder.length = ${exponentialOneSampleSketches_InOrder.length}")
	println(s"exponentialOneSampleSketches_Batch.length = ${exponentialOneSampleSketches_Batch.length}")
	println(s"exponentialMultiSampleSketches.length = ${exponentialMultiSampleSketches.length}")

	val exponentialColors = List(HTMLNamedColors.red, HTMLNamedColors.green, HTMLNamedColors.orange, HTMLNamedColors
		.purple, HTMLNamedColors.blue)

	plotDensities(exponentialsIncrementalMove, givenColorSeq = Some(exponentialColors), titleName = Some("Exponential Densities"))

	// TODO debugging sampling problem from the one-sample exponentials - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for single 5 exponential single-sample sketches (no reps)")

	assert(exponentialOneSampleSketches.length == exponentialsIncrementalMove.length, "Single-sample exponentials with no reps have same " +
		"length as given number of exponentials")

	exponentialsIncrementalMove.zip(exponentialOneSampleSketches).foreach {
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}


	println("---> Sample data for last 5 sketches from InOrder Repetitions of Exponential")
	val lastExponentialSet = exponentialOneSampleSketches_InOrder.takeRight(exponentialsIncrementalMove.length)

	assert(lastExponentialSet.length == exponentialsIncrementalMove.length, "Last of each exponential sketch set length must " +
		"equal exponentials incremental move length")

	exponentialsIncrementalMove.zip(lastExponentialSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of Exponential")
	// Taking the last sketch of each kind of exponential:
	val lastOfEachExponentialSketch: List[Sketch[Double]] =	exponentialOneSampleSketches_Batch.indices.map(_ +1)
		.zip(exponentialOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachExponentialSketch.length == exponentialsIncrementalMove.length, "Last of each exponential sketch length must " +
		"equal exponentials incremental move length")

	exponentialsIncrementalMove.zip(lastOfEachExponentialSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		exponentialOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		exponentialOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/


	plotSketchHistSplines(exponentialOneSampleSketches,
		titleName = Some(s"One-single Sample: Exponential sketches (No Reps)"),
		givenColorSeq = Some(exponentialColors),
		graphToColorLabels = Some(exponentialsIncrementalMove.map(_.toString)),
		originalDists = exponentialsIncrementalMove,
		overlayMixture = true
	)


	// First before plotting, reduce the exponentials!
	plotSketchHistSplines(lastExponentialSet,
		titleName = Some(s"One-single Sample: Exponential sketches (Reps: In Order)"),
		givenColorSeq = Some(exponentialColors),
		graphToColorLabels = Some(exponentialsIncrementalMove.map(_.toString)),
		originalDists = exponentialsIncrementalMove,
		overlayMixture = false
	)

	// Now plotting each element in the exponential set individually to see if they are the same
	val labels = exponentialsIncrementalMove.map(_.toString)

	lastExponentialSet.zip(exponentialColors).zip(labels) //.zip(exponentialsIncrementalMove)
		.map{ case ((skt, c), lab) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: Exponential Sketch Individual (Reps: In Order)"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = exponentialsIncrementalMove,
				overlayMixture = true
			)}

	plotSketchHistSplines(lastOfEachExponentialSketch,
		titleName = Some(s"One-single Sample: Exponential sketches (Reps: Batch)"),
		givenColorSeq = Some(exponentialColors),
		graphToColorLabels = Some(exponentialsIncrementalMove.map(_.toString)),
		originalDists = exponentialsIncrementalMove,
		overlayMixture = true
	)


	plotSketchHistSplines(exponentialMultiSampleSketches,
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(exponentialsIncrementalMove.map(_.toString)),
		givenColorSeq = Some(exponentialColors),
		originalDists = exponentialsIncrementalMove,
		overlayMixture = true
	)


	/*plotSketchHistSplineWithDists(exponentialMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green exponential", "red exponential", "purple exponential", "orange exponential", "blue exponential")),
		originalDists = exponentialsIncrementalMove
	)*/

}
