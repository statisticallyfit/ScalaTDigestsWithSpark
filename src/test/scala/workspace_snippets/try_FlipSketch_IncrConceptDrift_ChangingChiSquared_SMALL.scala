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
import util.distributionExtensions.instances._
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
object try_FlipSketch_IncrConceptDrift_ChangingChiSquared_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 8000 // was 1000 - to be like the original flip example


	val df1 = 3
	val df2 = 8
	val df3 = 24
	val df4 = 54
	val df5 = 100

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenChiSquared: ChiSquareDist = ChiSquareDist(df1)
	val redChiSquared: ChiSquareDist = ChiSquareDist(df2)
	val orangeChiSquared = ChiSquareDist(df3)
	val purpleChiSquared = ChiSquareDist(df4)
	val blueChiSquared = ChiSquareDist(df5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located chiSquareds.
	val chiSquaredsIncrementalMove: Seq[ChiSquareDist] = List(greenChiSquared, redChiSquared, orangeChiSquared,
		purpleChiSquared, blueChiSquared)

	// TODO Increasing amount of chiSquared distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / chiSquaredsIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val chiSquaredRepsInOrder: Seq[ChiSquareDist] = List.fill[List[ChiSquareDist]](numRepsPerDist)(chiSquaredsIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val chiSquaredRepsBatchKinds: Seq[ChiSquareDist] = chiSquaredsIncrementalMove.map(g => List.fill[ChiSquareDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val chiSquaredOneSampleData: List[Double] = chiSquaredsIncrementalMove.map(_.sample).toList
	val chiSquaredOneSampleData_InOrder: List[Double] = chiSquaredRepsInOrder.map(_.sample).toList
	val chiSquaredOneSampleData_Batch: List[Double] = chiSquaredRepsBatchKinds.map(_.sample).toList

	//val chiSquaredMultiSampleData: List[List[Double]] = chiSquaredsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val chiSquaredMultiSampleData: List[List[Double]] = chiSquaredsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch0_inorder: Sketch[Double] = Sketch.empty[Double]
	val sketch0_batch: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(chiSquaredDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val chiSquaredOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(chiSquaredOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val chiSquaredOneSampleSketches: Seq[Sketch[Double]] = sketch0.updateTrace(chiSquaredOneSampleData)
	val chiSquaredOneSampleSketches_InOrder: Seq[Sketch[Double]] = sketch0_inorder.updateTrace(chiSquaredOneSampleData_InOrder)
	val chiSquaredOneSampleSketches_Batch: Seq[Sketch[Double]] = sketch0_batch.updateTrace(chiSquaredOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val chiSquaredMultiSampleSketches: Seq[Sketch[Double]] = sketch00.updateWithMany(chiSquaredMultiSampleData)


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"chiSquaredsIncrementalMove.length = ${chiSquaredsIncrementalMove.length}")
	println(s"chiSquaredOneSampleSketches.length = ${chiSquaredOneSampleSketches.length}")
	println(s"chiSquaredOneSampleSketches_InOrder.length = ${chiSquaredOneSampleSketches_InOrder.length}")
	println(s"chiSquaredOneSampleSketches_Batch.length = ${chiSquaredOneSampleSketches_Batch.length}")
	println(s"chiSquaredMultiSampleSketches.length = ${chiSquaredMultiSampleSketches.length}")

	val chiSquaredColors = List(HTMLNamedColors.red, HTMLNamedColors.green, HTMLNamedColors.orange, HTMLNamedColors
		.purple, HTMLNamedColors.blue)

	plotDensities(chiSquaredsIncrementalMove, givenColorSeq = Some(chiSquaredColors), titleName = Some("ChiSquared Densities"))

	// TODO debugging sampling problem from the one-sample chiSquareds - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for single 5 chiSquared single-sample sketches (no reps)")

	assert(chiSquaredOneSampleSketches.length == chiSquaredsIncrementalMove.length, "Single-sample chiSquareds with no reps have same " +
		"length as given number of chiSquareds")

	chiSquaredsIncrementalMove.zip(chiSquaredOneSampleSketches).foreach {
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}


	println("---> Sample data for last 5 sketches from InOrder Repetitions of ChiSquared")
	val lastChiSquaredSet = chiSquaredOneSampleSketches_InOrder.takeRight(chiSquaredsIncrementalMove.length)

	assert(lastChiSquaredSet.length == chiSquaredsIncrementalMove.length, "Last of each chiSquared sketch set length must " +
		"equal chiSquareds incremental move length")

	chiSquaredsIncrementalMove.zip(lastChiSquaredSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of ChiSquared")
	// Taking the last sketch of each kind of chiSquared:
	val lastOfEachChiSquaredSketch: List[Sketch[Double]] =	chiSquaredOneSampleSketches_Batch.indices.map(_ +1)
		.zip(chiSquaredOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachChiSquaredSketch.length == chiSquaredsIncrementalMove.length, "Last of each chiSquared sketch length must " +
		"equal chiSquareds incremental move length")

	chiSquaredsIncrementalMove.zip(lastOfEachChiSquaredSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		chiSquaredOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		chiSquaredOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/


	plotSketchHistSplines[Double, ChiSquareDist](chiSquaredOneSampleSketches,
		titleName = Some(s"One-single Sample: ChiSquared sketches (No Reps)"),
		givenColorSeq = Some(chiSquaredColors),
		graphToColorLabels = Some(chiSquaredsIncrementalMove.map(_.toString)),
		originalDists = chiSquaredsIncrementalMove,
		overlayMixture = true
	)


	// First before plotting, reduce the chiSquareds!
	plotSketchHistSplines(lastChiSquaredSet,
		titleName = Some(s"One-single Sample: ChiSquared sketches (Reps: In Order)"),
		givenColorSeq = Some(chiSquaredColors),
		graphToColorLabels = Some(chiSquaredsIncrementalMove.map(_.toString)),
		originalDists = chiSquaredsIncrementalMove,
		overlayMixture = true
	)

	// Now plotting each element in the chiSquared set individually to see if they are the same
	val labels = chiSquaredsIncrementalMove.map(_.toString)

	lastChiSquaredSet.zip(chiSquaredColors).zip(labels) //.zip(chiSquaredsIncrementalMove)
		.map{ case ((skt, c), lab) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: ChiSquared Sketch Individual (Reps: In Order)"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = chiSquaredsIncrementalMove,
				overlayMixture = true
			)}

	plotSketchHistSplines(lastOfEachChiSquaredSketch,
		titleName = Some(s"One-single Sample: ChiSquared sketches (Reps: Batch)"),
		givenColorSeq = Some(chiSquaredColors),
		graphToColorLabels = Some(chiSquaredsIncrementalMove.map(_.toString)),
		originalDists = chiSquaredsIncrementalMove,
		overlayMixture = true
	)


	plotSketchHistSplines(chiSquaredMultiSampleSketches,
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(chiSquaredsIncrementalMove.map(_.toString)),
		givenColorSeq = Some(chiSquaredColors),
		originalDists = chiSquaredsIncrementalMove,
		overlayMixture = true
	)


	/*plotSketchHistSplineWithDists(chiSquaredMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green chiSquared", "red chiSquared", "purple chiSquared", "orange chiSquared", "blue chiSquared")),
		originalDists = chiSquaredsIncrementalMove
	)*/

}
