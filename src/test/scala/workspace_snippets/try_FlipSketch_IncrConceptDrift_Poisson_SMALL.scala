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
object try_FlipSketch_IncrConceptDrift_Poisson_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 8000 // was 1000 - to be like the original flip example


	val lambda1 = 4
	val lambda2 = 22
	val lambda3 = 52
	val lambda4 = 96
	val lambda5 = 150

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenPoisson: PoissonDist = PoissonDist(lambda1)
	val redPoisson: PoissonDist = PoissonDist(lambda2)
	val purplePoisson = PoissonDist(lambda3)
	val orangePoisson = PoissonDist(lambda4)
	val bluePoisson = PoissonDist(lambda5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located poissons.
	val poissonsIncrementalMove: Seq[PoissonDist] = List(greenPoisson, redPoisson, purplePoisson,
		orangePoisson, bluePoisson)

	// TODO Increasing amount of poisson distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / poissonsIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val poissonRepsInOrder: Seq[PoissonDist] = List.fill[List[PoissonDist]](numRepsPerDist)(poissonsIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val poissonRepsBatchKinds: Seq[PoissonDist] = poissonsIncrementalMove.map(g => List.fill[PoissonDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val poissonOneSampleData: List[Int] = poissonsIncrementalMove.map(_.sample).toList
	val poissonOneSampleData_InOrder: List[Int] = poissonRepsInOrder.map(_.sample).toList
	val poissonOneSampleData_Batch: List[Int] = poissonRepsBatchKinds.map(_.sample).toList

	//val poissonMultiSampleData: List[List[Int]] = poissonsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val poissonMultiSampleData: List[List[Int]] = poissonsIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Int] = Sketch.empty[Int]
	val sketch0_inorder: Sketch[Int] = Sketch.empty[Int]
	val sketch0_batch: Sketch[Int] = Sketch.empty[Int]
	val sketch00: Sketch[Int] = Sketch.empty[Int]
	//val firstTD: TDigest = TDigest.sketch(poissonDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val poissonOneSampleSketches: Seq[Sketch[Int]] = (sketch0 :: sketch0.updateTrace(poissonOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val poissonOneSampleSketches: Seq[Sketch[Int]] = sketch0.updateTrace(poissonOneSampleData)
	val poissonOneSampleSketches_InOrder: Seq[Sketch[Int]] = sketch0_inorder.updateTrace(poissonOneSampleData_InOrder)
	val poissonOneSampleSketches_Batch: Seq[Sketch[Int]] = sketch0_batch.updateTrace(poissonOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val poissonMultiSampleSketches: Seq[Sketch[Int]] = sketch00.updateWithMany(poissonMultiSampleData)


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"poissonsIncrementalMove.length = ${poissonsIncrementalMove.length}")
	println(s"poissonOneSampleSketches.length = ${poissonOneSampleSketches.length}")
	println(s"poissonOneSampleSketches_InOrder.length = ${poissonOneSampleSketches_InOrder.length}")
	println(s"poissonOneSampleSketches_Batch.length = ${poissonOneSampleSketches_Batch.length}")
	println(s"poissonMultiSampleSketches.length = ${poissonMultiSampleSketches.length}")

	val poissonColors = List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
		.orange, HTMLNamedColors.blue)

	plotDensities(poissonsIncrementalMove, givenColorSeq = Some(poissonColors), titleName = Some("Poisson Densities"))

	// TODO debugging sampling problem from the one-sample poissons - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for single 5 poisson single-sample sketches (no reps)")

	assert(poissonOneSampleSketches.length == poissonsIncrementalMove.length, "Single-sample poissons with no reps have same " +
		"length as given number of poissons")

	poissonsIncrementalMove.zip(poissonOneSampleSketches).foreach {
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}


	println("---> Sample data for last 5 sketches from InOrder Repetitions of Poisson")
	val lastPoissonSet = poissonOneSampleSketches_InOrder.takeRight(poissonsIncrementalMove.length)

	assert(lastPoissonSet.length == poissonsIncrementalMove.length, "Last of each poisson sketch set length must " +
		"equal poissons incremental move length")

	poissonsIncrementalMove.zip(lastPoissonSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of Poisson")
	// Taking the last sketch of each kind of poisson:
	val lastOfEachPoissonSketch: List[Sketch[Int]] =	poissonOneSampleSketches_Batch.indices.map(_ +1)
		.zip(poissonOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachPoissonSketch.length == poissonsIncrementalMove.length, "Last of each poisson sketch length must " +
		"equal poissons incremental move length")

	poissonsIncrementalMove.zip(lastOfEachPoissonSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		poissonOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		poissonOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/

	// ------------------------------------------------------------------------------------

	// TODO stuck here - need to conver the sketch[int] to sketch[intz] but that would take a lot of time for
	//  computer???
	// TODO make implicit int -> intz, double -> real
	/*implicit def convertIntToBigInt(skts: Seq[Sketch[Int]]): Seq[Sketch[IntZ]] = {
		skts.map(skt => skt.map(x => BigInt.int2bigInt(x)))
	}*/

	// TODO just commenting out because of xMin < xMax error (one sample cause?)
	/*plotSketchHistSplines(poissonOneSampleSketches,
		titleName = Some(s"One-single Sample: Poisson sketches (No Reps)"),
		givenColorSeq = Some(poissonColors),
		graphToColorLabels = Some(poissonsIncrementalMove.map(_.toString)),
		originalDists = poissonsIncrementalMove,
		overlayMixture = true
	)*/


	// First before plotting, reduce the poissons!
	plotSketchHistSplines(lastPoissonSet,
		titleName = Some(s"One-single Sample: Poisson sketches (Reps: In Order)"),
		givenColorSeq = Some(poissonColors),
		graphToColorLabels = Some(poissonsIncrementalMove.map(_.toString)),
		originalDists = poissonsIncrementalMove,
		overlayMixture = false
	)

	// Now plotting each element in the poisson set individually to see if they are the same
	val labels = poissonsIncrementalMove.map(_.toString)

	lastPoissonSet.zip(poissonColors).zip(labels) //.zip(poissonsIncrementalMove)
		.map{ case ((skt, c), lab) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: Poisson Sketch Individual (Reps: In Order)"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = poissonsIncrementalMove,
				overlayMixture = false
			)}

	plotSketchHistSplines(lastOfEachPoissonSketch,
		titleName = Some(s"One-single Sample: Poisson sketches (Reps: Batch)"),
		givenColorSeq = Some(poissonColors),
		graphToColorLabels = Some(poissonsIncrementalMove.map(_.toString)),
		originalDists = poissonsIncrementalMove,
		overlayMixture = false
	)


	plotSketchHistSplines(poissonMultiSampleSketches,
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(poissonsIncrementalMove.map(_.toString)),
		givenColorSeq = Some(poissonColors),
		originalDists = poissonsIncrementalMove,
		overlayMixture = false
	)


	/*plotSketchHistSplineWithDists(poissonMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green poisson", "red poisson", "purple poisson", "orange poisson", "blue poisson")),
		originalDists = poissonsIncrementalMove
	)*/

}
