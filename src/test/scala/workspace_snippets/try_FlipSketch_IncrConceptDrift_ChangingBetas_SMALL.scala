package workspace_snippets



import com.cibo.evilplot.colors.{Color, HTMLNamedColors}


import flip.implicits._
import flip.pdf.Sketch
import util.TMeasure._

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
object try_FlipSketch_IncrConceptDrift_ChangingBetas_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 10000 // was 1000 - to be like the original flip example


	val (a1, b1) = (3, 68)
	val (a2, b2) = (30, 124)
	val (a3, b3) = (28, 44)
	val (a4, b4) = (13, 6) // (168, 12) made this crash // good (93, 13)
	val (a5, b5) = (57, 5)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val redBeta: BetaDist = BetaDist(a1, b1)
	val greenBeta: BetaDist = BetaDist(a2, b2)
	val orangeBeta = BetaDist(a3, b3)
	val purpleBeta = BetaDist(a4, b4)
	val blueBeta = BetaDist(a5, b5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located betas.
	val betasIncrementalMove: Seq[BetaDist] = List(redBeta, greenBeta, orangeBeta, purpleBeta, blueBeta)

	// TODO Increasing amount of beta distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / betasIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val betaRepsInOrder: Seq[BetaDist] = List.fill[List[BetaDist]](numRepsPerDist)(betasIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val betaRepsBatchKinds: Seq[BetaDist] = betasIncrementalMove.map(g => List.fill[BetaDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val betaOneSampleData: List[Double] = betasIncrementalMove.map(_.sample).toList
	val betaOneSampleData_InOrder: List[Double] = betaRepsInOrder.map(_.sample).toList
	val betaOneSampleData_Batch: List[Double] = betaRepsBatchKinds.map(_.sample).toList

	//val betaMultiSampleData: List[List[Double]] = betasIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val betaMultiSampleData: List[List[Double]] = betasIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch0_inorder: Sketch[Double] = Sketch.empty[Double]
	val sketch0_batch: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(betaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val betaOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(betaOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val betaOneSampleSketches: Seq[Sketch[Double]] = sketch0.updateTrace(betaOneSampleData)
	val betaOneSampleSketches_InOrder: Seq[Sketch[Double]] = sketch0_inorder.updateTrace(betaOneSampleData_InOrder)
	val betaOneSampleSketches_Batch: Seq[Sketch[Double]] = sketch0_batch.updateTrace(betaOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val betaMultiSampleSketches: Seq[Sketch[Double]] = sketch00.updateWithMany(betaMultiSampleData)


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"betasIncrementalMove.length = ${betasIncrementalMove.length}")
	println(s"betaOneSampleSketches.length = ${betaOneSampleSketches.length}")
	println(s"betaOneSampleSketches_InOrder.length = ${betaOneSampleSketches_InOrder.length}")
	println(s"betaOneSampleSketches_Batch.length = ${betaOneSampleSketches_Batch.length}")
	println(s"betaMultiSampleSketches.length = ${betaMultiSampleSketches.length}")

	val betaColors = List(HTMLNamedColors.red, HTMLNamedColors.green, HTMLNamedColors.orange, HTMLNamedColors
		.purple, HTMLNamedColors.blue)

	plotDensities(betasIncrementalMove, givenColorSeq = Some(betaColors), titleName = Some("Beta Densities"))

	// TODO debugging sampling problem from the one-sample betas - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for single 5 beta single-sample sketches (no reps)")

	assert(betaOneSampleSketches.length == betasIncrementalMove.length, "Single-sample betas with no reps have same " +
		"length as given number of betas")

	betasIncrementalMove.zip(betaOneSampleSketches).foreach {
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}


	println("---> Sample data for last 5 sketches from InOrder Repetitions of Beta")
	val lastBetaSet = betaOneSampleSketches_InOrder.takeRight(betasIncrementalMove.length)

	assert(lastBetaSet.length == betasIncrementalMove.length, "Last of each beta sketch set length must " +
		"equal betas incremental move length")

	betasIncrementalMove.zip(lastBetaSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of Beta")
	// Taking the last sketch of each kind of beta:
	val lastOfEachBetaSketch: List[Sketch[Double]] =	betaOneSampleSketches_Batch.indices.map(_ +1)
		.zip(betaOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachBetaSketch.length == betasIncrementalMove.length, "Last of each beta sketch length must " +
		"equal betas incremental move length")

	betasIncrementalMove.zip(lastOfEachBetaSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		betaOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		betaOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/


	plotSketchHistSplines(betaOneSampleSketches,
		titleName = Some(s"One-single Sample: Beta sketches (No Reps)"),
		givenColorSeq = Some(betaColors),
		graphToColorLabels = Some(betasIncrementalMove.map(_.toString)),
		originalDists = betasIncrementalMove,
		overlayMixture = false
	)


	// First before plotting, reduce the betas!
	plotSketchHistSplines(lastBetaSet,
		titleName = Some(s"One-single Sample: Beta sketches (Reps: In Order)"),
		givenColorSeq = Some(betaColors),
		graphToColorLabels = Some(betasIncrementalMove.map(_.toString)),
		originalDists = betasIncrementalMove,
		overlayMixture = false
	)

	// Now plotting each element in the beta set individually to see if they are the same
	val labels = betasIncrementalMove.map(_.toString)

	lastBetaSet.zip(betaColors).zip(labels) //.zip(betasIncrementalMove)
		.map{ case ((skt, c), lab) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: Beta Sketch Individual (Reps: In Order)"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = betasIncrementalMove,
				overlayMixture = false
			)}

	plotSketchHistSplines(lastOfEachBetaSketch,
		titleName = Some(s"One-single Sample: Beta sketches (Reps: Batch)"),
		givenColorSeq = Some(betaColors),
		graphToColorLabels = Some(betasIncrementalMove.map(_.toString)),
		originalDists = betasIncrementalMove,
		overlayMixture = false
	)


	plotSketchHistSplines(betaMultiSampleSketches,
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(betasIncrementalMove.map(_.toString)),
		givenColorSeq = Some(betaColors),
		originalDists = betasIncrementalMove,
		overlayMixture = false
	)


	/*plotSketchHistSplineWithDists(betaMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green beta", "red beta", "purple beta", "orange beta", "blue beta")),
		originalDists = betasIncrementalMove
	)*/

}
