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
object try_FlipSketch_IncrConceptDrift_Binomial_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 8000 // was 1000 - to be like the original flip example


	val (n1, p1) = (14, 0.06)
	val (n2, p2) = (60, 0.17)
	val (n3, p3) = (90, 0.35)
	val (n4, p4) = (114, 0.426)
	val (n5, p5) = (151, 0.66)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenBinomial: BinomialDist = BinomialDist(n1, p1)
	val redBinomial: BinomialDist = BinomialDist(n2, p2)
	val purpleBinomial = BinomialDist(n3, p3)
	val orangeBinomial = BinomialDist(n4, p4)
	val blueBinomial = BinomialDist(n5, p5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located binomials.
	val binomialsIncrementalMove: Seq[BinomialDist] = List(greenBinomial, redBinomial, purpleBinomial,
		orangeBinomial, blueBinomial)

	// TODO Increasing amount of binomial distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / binomialsIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val binomialRepsInOrder: Seq[BinomialDist] = List.fill[List[BinomialDist]](numRepsPerDist)(binomialsIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val binomialRepsBatchKinds: Seq[BinomialDist] = binomialsIncrementalMove.map(g => List.fill[BinomialDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val binomialOneSampleData: List[Int] = binomialsIncrementalMove.map(_.sample).toList
	val binomialOneSampleData_InOrder: List[Int] = binomialRepsInOrder.map(_.sample).toList
	val binomialOneSampleData_Batch: List[Int] = binomialRepsBatchKinds.map(_.sample).toList

	//val binomialMultiSampleData: List[List[Int]] = binomialsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val binomialMultiSampleData: List[List[Int]] = binomialsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Int] = Sketch.empty[Int]
	val sketch0_inorder: Sketch[Int] = Sketch.empty[Int]
	val sketch0_batch: Sketch[Int] = Sketch.empty[Int]
	val sketch00: Sketch[Int] = Sketch.empty[Int]
	//val firstTD: TDigest = TDigest.sketch(binomialDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val binomialOneSampleSketches: Seq[Sketch[Int]] = (sketch0 :: sketch0.updateTrace(binomialOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val binomialOneSampleSketches: Seq[Sketch[Int]] = sketch0.updateTrace(binomialOneSampleData)
	val binomialOneSampleSketches_InOrder: Seq[Sketch[Int]] = sketch0_inorder.updateTrace(binomialOneSampleData_InOrder)
	val binomialOneSampleSketches_Batch: Seq[Sketch[Int]] = sketch0_batch.updateTrace(binomialOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val binomialMultiSampleSketches: Seq[Sketch[Int]] = sketch00.updateWithMany(binomialMultiSampleData)


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"binomialsIncrementalMove.length = ${binomialsIncrementalMove.length}")
	println(s"binomialOneSampleSketches.length = ${binomialOneSampleSketches.length}")
	println(s"binomialOneSampleSketches_InOrder.length = ${binomialOneSampleSketches_InOrder.length}")
	println(s"binomialOneSampleSketches_Batch.length = ${binomialOneSampleSketches_Batch.length}")
	println(s"binomialMultiSampleSketches.length = ${binomialMultiSampleSketches.length}")

	val binomialColors = List(HTMLNamedColors.red, HTMLNamedColors.green, HTMLNamedColors.orange, HTMLNamedColors
		.purple, HTMLNamedColors.blue)

	plotDensities(binomialsIncrementalMove, givenColorSeq = Some(binomialColors), titleName = Some("Binomial Densities"))

	// TODO debugging sampling problem from the one-sample binomials - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for single 5 binomial single-sample sketches (no reps)")

	assert(binomialOneSampleSketches.length == binomialsIncrementalMove.length, "Single-sample binomials with no reps have same " +
		"length as given number of binomials")

	binomialsIncrementalMove.zip(binomialOneSampleSketches).foreach {
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}


	println("---> Sample data for last 5 sketches from InOrder Repetitions of Binomial")
	val lastBinomialSet = binomialOneSampleSketches_InOrder.takeRight(binomialsIncrementalMove.length)

	assert(lastBinomialSet.length == binomialsIncrementalMove.length, "Last of each binomial sketch set length must " +
		"equal binomials incremental move length")

	binomialsIncrementalMove.zip(lastBinomialSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of Binomial")
	// Taking the last sketch of each kind of binomial:
	val lastOfEachBinomialSketch: List[Sketch[Int]] =	binomialOneSampleSketches_Batch.indices.map(_ +1)
		.zip(binomialOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachBinomialSketch.length == binomialsIncrementalMove.length, "Last of each binomial sketch length must " +
		"equal binomials incremental move length")

	binomialsIncrementalMove.zip(lastOfEachBinomialSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		binomialOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		binomialOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/

	// ------------------------------------------------------------------------------------

	// TODO stuck here - need to conver the sketch[int] to sketch[intz] but that would take a lot of time for
	//  computer???
	// TODO make implicit int -> intz, double -> real
	/*implicit def convertIntToBigInt(skts: Seq[Sketch[Int]]): Seq[Sketch[IntZ]] = {
		skts.map(skt => skt.map(x => BigInt.int2bigInt(x)))
	}*/

	// TODO just commenting out because of xMin < xMax error (one sample cause?)
	/*plotSketchHistSplines(binomialOneSampleSketches,
		titleName = Some(s"One-single Sample: Binomial sketches (No Reps)"),
		givenColorSeq = Some(binomialColors),
		graphToColorLabels = Some(binomialsIncrementalMove.map(_.toString)),
		originalDists = binomialsIncrementalMove,
		overlayMixture = true
	)*/


	// First before plotting, reduce the binomials!
	plotSketchHistSplines(lastBinomialSet,
		titleName = Some(s"One-single Sample: Binomial sketches (Reps: In Order)"),
		givenColorSeq = Some(binomialColors),
		graphToColorLabels = Some(binomialsIncrementalMove.map(_.toString)),
		originalDists = binomialsIncrementalMove,
		overlayMixture = true
	)

	// Now plotting each element in the binomial set individually to see if they are the same
	val labels = binomialsIncrementalMove.map(_.toString)

	lastBinomialSet.zip(binomialColors).zip(labels) //.zip(binomialsIncrementalMove)
		.map{ case ((skt, c), lab) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: Binomial Sketch Individual (Reps: In Order)"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = binomialsIncrementalMove,
				overlayMixture = true
			)}

	plotSketchHistSplines(lastOfEachBinomialSketch,
		titleName = Some(s"One-single Sample: Binomial sketches (Reps: Batch)"),
		givenColorSeq = Some(binomialColors),
		graphToColorLabels = Some(binomialsIncrementalMove.map(_.toString)),
		originalDists = binomialsIncrementalMove,
		overlayMixture = true
	)


	plotSketchHistSplines(binomialMultiSampleSketches,
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(binomialsIncrementalMove.map(_.toString)),
		givenColorSeq = Some(binomialColors),
		originalDists = binomialsIncrementalMove,
		overlayMixture = true
	)


	/*plotSketchHistSplineWithDists(binomialMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green binomial", "red binomial", "purple binomial", "orange binomial", "blue binomial")),
		originalDists = binomialsIncrementalMove
	)*/

}
