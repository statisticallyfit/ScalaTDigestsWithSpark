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
//import util.distributionExtensions.syntax._


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
object try_FlipSketch_IncrConceptDrift_ChangingWeibulls_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 8000 // was 1000 - to be like the original flip example


	val (a1, b1) = (6, 0.4)
	val (a2, b2) = (9.6, 1.2)
	val (a3, b3) = (10.2, 3.1)
	val (a4, b4) = (40, 5)
	val (a5, b5) = (30.6, 9.2)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenWeibull: WeibullDist = WeibullDist(a1, b1)
	val redWeibull: WeibullDist = WeibullDist(a2, b2)
	val purpleWeibull: WeibullDist = WeibullDist(a3, b3)
	val orangeWeibull: WeibullDist = WeibullDist(a4, b4)
	val blueWeibull: WeibullDist = WeibullDist(a5, b5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located weibulls.
	val weibullsIncrementalMove: Seq[WeibullDist] = List(redWeibull, greenWeibull, orangeWeibull, purpleWeibull, blueWeibull)

	// TODO Increasing amount of weibull distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / weibullsIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val weibullRepsInOrder: Seq[WeibullDist] = List.fill[List[WeibullDist]](numRepsPerDist)(weibullsIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val weibullRepsBatchKinds: Seq[WeibullDist] = weibullsIncrementalMove.map(g => List.fill[WeibullDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val weibullOneSampleData: List[Double] = weibullsIncrementalMove.map(_.sample).toList
	val weibullOneSampleData_InOrder: List[Double] = weibullRepsInOrder.map(_.sample).toList
	val weibullOneSampleData_Batch: List[Double] = weibullRepsBatchKinds.map(_.sample).toList

	//val weibullMultiSampleData: List[List[Double]] = weibullsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val weibullMultiSampleData: List[List[Double]] = weibullsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch0_inorder: Sketch[Double] = Sketch.empty[Double]
	val sketch0_batch: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(weibullDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val weibullOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(weibullOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val weibullOneSampleSketches: Seq[Sketch[Double]] = sketch0.updateTrace(weibullOneSampleData)
	val weibullOneSampleSketches_InOrder: Seq[Sketch[Double]] = sketch0_inorder.updateTrace(weibullOneSampleData_InOrder)
	val weibullOneSampleSketches_Batch: Seq[Sketch[Double]] = sketch0_batch.updateTrace(weibullOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val weibullMultiSampleSketches: Seq[Sketch[Double]] = sketch00.updateWithMany(weibullMultiSampleData)


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"weibullsIncrementalMove.length = ${weibullsIncrementalMove.length}")
	println(s"weibullOneSampleSketches.length = ${weibullOneSampleSketches.length}")
	println(s"weibullOneSampleSketches_InOrder.length = ${weibullOneSampleSketches_InOrder.length}")
	println(s"weibullOneSampleSketches_Batch.length = ${weibullOneSampleSketches_Batch.length}")
	println(s"weibullMultiSampleSketches.length = ${weibullMultiSampleSketches.length}")

	val weibullColors = List(HTMLNamedColors.red, HTMLNamedColors.green, HTMLNamedColors.orange, HTMLNamedColors
		.purple, HTMLNamedColors.blue)

	plotDensities(weibullsIncrementalMove, givenColorSeq = Some(weibullColors), titleName = Some("Weibull Densities"))

	// TODO debugging sampling problem from the one-sample weibulls - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for single 5 weibull single-sample sketches (no reps)")

	assert(weibullOneSampleSketches.length == weibullsIncrementalMove.length, "Single-sample weibulls with no reps have same " +
		"length as given number of weibulls")

	weibullsIncrementalMove.zip(weibullOneSampleSketches).foreach {
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}


	println("---> Sample data for last 5 sketches from InOrder Repetitions of Weibull")
	val lastWeibullSet = weibullOneSampleSketches_InOrder.takeRight(weibullsIncrementalMove.length)

	assert(lastWeibullSet.length == weibullsIncrementalMove.length, "Last of each weibull sketch set length must " +
		"equal weibulls incremental move length")

	weibullsIncrementalMove.zip(lastWeibullSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of Weibull")
	// Taking the last sketch of each kind of weibull:
	val lastOfEachWeibullSketch: List[Sketch[Double]] =	weibullOneSampleSketches_Batch.indices.map(_ +1)
		.zip(weibullOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachWeibullSketch.length == weibullsIncrementalMove.length, "Last of each weibull sketch length must " +
		"equal weibulls incremental move length")

	weibullsIncrementalMove.zip(lastOfEachWeibullSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		weibullOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		weibullOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/


	plotSketchHistSplines(weibullOneSampleSketches,
		titleName = Some(s"One-single Sample: Weibull sketches (No Reps)"),
		givenColorSeq = Some(weibullColors),
		graphToColorLabels = Some(weibullsIncrementalMove.map(_.toString)),
		originalDists = weibullsIncrementalMove,
		overlayMixture = true
	)


	// First before plotting, reduce the weibulls!
	plotSketchHistSplines(lastWeibullSet,
		titleName = Some(s"One-single Sample: Weibull sketches (Reps: In Order)"),
		givenColorSeq = Some(weibullColors),
		graphToColorLabels = Some(weibullsIncrementalMove.map(_.toString)),
		originalDists = weibullsIncrementalMove,
		overlayMixture = true
	)

	// Now plotting each element in the weibull set individually to see if they are the same
	val labels = weibullsIncrementalMove.map(_.toString)

	lastWeibullSet.zip(weibullColors).zip(labels) //.zip(weibullsIncrementalMove)
		.map{ case ((skt, c), lab) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: Weibull Sketch Individual (Reps: In Order)"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = weibullsIncrementalMove,
				overlayMixture = true
			)}

	plotSketchHistSplines(lastOfEachWeibullSketch,
		titleName = Some(s"One-single Sample: Weibull sketches (Reps: Batch)"),
		givenColorSeq = Some(weibullColors),
		graphToColorLabels = Some(weibullsIncrementalMove.map(_.toString)),
		originalDists = weibullsIncrementalMove,
		overlayMixture = true
	)


	plotSketchHistSplines(weibullMultiSampleSketches,
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(weibullsIncrementalMove.map(_.toString)),
		givenColorSeq = Some(weibullColors),
		originalDists = weibullsIncrementalMove,
		overlayMixture = true
	)


	/*plotSketchHistSplineWithDists(weibullMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green weibull", "red weibull", "purple weibull", "orange weibull", "blue weibull")),
		originalDists = weibullsIncrementalMove
	)*/

}
