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
object try_FlipSketch_IncrConceptDrift_ChangingGammas_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	/*val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01*/

	val SAMPLE_SIZE = 1000 //4000 //500 //8000 // was 1000


	val (a1, b1) = (9, 33)
	val (a2, b2) = (47, 49)
	val (a3, b3) = (92, 26)
	val (a4, b4) = (83, 13) // (168, 12) made this crash // good (93, 13)
	val (a5, b5) = (90, 8)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenGamma: GammaDist = GammaDist(a1, b1)
	val redGamma: GammaDist = GammaDist(a2, b2)
	val purpleGamma = GammaDist(a3, b3)
	val orangeGamma = GammaDist(a4, b4)
	val blueGamma = GammaDist(a5, b5)

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located gammas.
	val gammasIncrementalMove: Seq[GammaDist] = List(greenGamma, redGamma, purpleGamma, orangeGamma, blueGamma)

	// TODO Increasing amount of gamma distributions to try to fix the one-sample estimation (all samples currently
	// same  number)
	val numRepsPerDist: Int = (dataNo * 1.0 / gammasIncrementalMove.length).toInt // to fit to dataNo and mirror
	// smile's  normal dist example
	// --> In order replications --> means "1,2,3,4", "1,2,3,4", ... and sketches will be created by receiving the
	// dists  in this given order
	val gammaRepsInOrder: Seq[GammaDist] = List.fill[List[GammaDist]](numRepsPerDist)(gammasIncrementalMove.toList)
		.flatten
	// --> Batch replications --> means "1,1,1,1", "2,2,2,2", ... and sketches will be created by receiving the
	// dists in this order.
	val gammaRepsBatchKinds: Seq[GammaDist] = gammasIncrementalMove.map(g => List.fill[GammaDist](numRepsPerDist)(g)
	).toList.flatten


	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	//val gammaOneSampleData: List[Double] = gammasIncrementalMove.map(_.sample).toList
	val gammaOneSampleData_InOrder: List[Double] = gammaRepsInOrder.map(_.sample).toList
	val gammaOneSampleData_Batch: List[Double] = gammaRepsBatchKinds.map(_.sample).toList

	//val gammaMultiSampleData: List[List[Double]] = gammasIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList
	val gammaMultiSampleData: List[List[Double]] = gammasIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0_inorder: Sketch[Double] = Sketch.empty[Double]
	val sketch0_batch: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(gammaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	//val gammaOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(gammaOneSampleData)).drop(1)
	// drop empty sketch at beginning
	val gammaOneSampleSketches_InOrder: Seq[Sketch[Double]] = sketch0_inorder.updateTrace(gammaOneSampleData_InOrder)
	val gammaOneSampleSketches_Batch: Seq[Sketch[Double]] = sketch0_batch.updateTrace(gammaOneSampleData_Batch)
	// TODO assert with kolmogorov smirnov next that the inorder-sketch result in mixture distribution like the
	//  multi sample sketches, and assert the batch-sketches result in non-mixture (single peaks) -- compare to
	//  original sample from original dist.

	// TODO assert with KS that batch-onesinglesample results in same mixture as multisample sketches (below)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val gammaMultiSampleSketches: Seq[Sketch[Double]] = (sketch00 :: sketch00.updateWithMany(gammaMultiSampleData))
		.drop(1) // drop empty sketch at beginning


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"gammasIncrementalMove.length = ${gammasIncrementalMove.length}")
	println(s"gammaOneSampleSketches_InOrder.length = ${gammaOneSampleSketches_InOrder.length}")
	println(s"gammaOneSampleSketches_Batch.length = ${gammaOneSampleSketches_Batch.length}")
	println(s"gammaMultiSampleSketches.length = ${gammaMultiSampleSketches.length}")


	plotDensities(gammasIncrementalMove,
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
	)

	// TODO debugging sampling problem from the one-sample gammas - why are all the sample points the same?  (-40?)
	//  for the first one?
	println("---> Printing the sample data for each sketch: ")

	println("---> Sample data for last 5 sketches from InOrder Repetitions of Gamma")
	val lastGammaSet = gammaOneSampleSketches_InOrder.takeRight(gammasIncrementalMove.length)

	assert(lastGammaSet.length == gammasIncrementalMove.length, "Last of each gamma sketch set length must " +
		"equal gammas incremental move length")

	gammasIncrementalMove.zip(lastGammaSet).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	println(s"---> Sample data for Every ${numRepsPerDist} (last) sketch from Batch Repetitions of Gamma")
	// Taking the last sketch of each kind of gamma:
	val lastOfEachGammaSketch: List[Sketch[Double]] =	gammaOneSampleSketches_Batch.indices.map(_ +1)
		.zip(gammaOneSampleSketches_Batch)
		.toList
		.filter{ case (idx, v) => idx % numRepsPerDist == 0}.unzip._2

	assert(lastOfEachGammaSketch.length == gammasIncrementalMove.length, "Last of each gamma sketch length must " +
		"equal gammas incremental move length")

	gammasIncrementalMove.zip(lastOfEachGammaSketch).foreach{
		case (g, skt) => println(s"${g.toString}: ${skt.samples(SAMPLE_SIZE)._2}")
	}

	/*println("---> Printing index marker at which sample data stops containing the same values throughout")


	import util.GeneralUtil._

	println(s"Index marker for InOrder Rep Sketches: ${
		gammaOneSampleSketches_InOrder.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")
	println(s"Index marker for Batch Rep Sketches: ${
		gammaOneSampleSketches_Batch.map(skt => indexOfFirstDifferentNum(skt.samples(SAMPLE_SIZE)._2))
	}")*/
	// First before plotting, reduce the gammas!
	plotSketchHistSplines(lastGammaSet,
		titleName = Some(s"One-single Sample: Gamma sketches (Reps: In Order)"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(gammasIncrementalMove.map(_.toString)),
		originalDists = Some(gammasIncrementalMove),
		overlayMixture = true
	)

	// Now plotting each element in the gamma set individually to see if they are the same
	val colors = List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
		.orange, HTMLNamedColors.blue)
	val labels = gammasIncrementalMove.map(_.toString)

	lastGammaSet.zip(colors).zip(labels).zip(gammasIncrementalMove)
		.map{ case (((skt, c), lab), gdist) =>
			plotSketchHistSplines(List(skt),
				titleName = Some(s"One Sample: Gamma Sketch Individual (Reps: In Order"),
				givenColorSeq = Some(List(c)),
				graphToColorLabels = Some(List(lab)),
				originalDists = Some(gammasIncrementalMove),
				overlayMixture = true
	)}

	plotSketchHistSplines(lastOfEachGammaSketch,
		titleName = Some(s"One-single Sample: Gamma sketches (Reps: Batch)"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(gammasIncrementalMove.map(_.toString)),
		originalDists = Some(gammasIncrementalMove),
		overlayMixture = true
	)


	plotSketchHistSplines(gammaMultiSampleSketches, //.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE (left out first 100 " +
			s"sketches)"),
		graphToColorLabels = Some(gammasIncrementalMove.map(_.toString)),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		originalDists = Some(gammasIncrementalMove),
		overlayMixture = true
	)


	/*plotSketchHistSplineWithDists(gammaMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green gamma", "red gamma", "purple gamma", "orange gamma", "blue gamma")),
		originalDists = gammasIncrementalMove
	)*/

}
