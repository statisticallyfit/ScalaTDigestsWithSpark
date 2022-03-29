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
 * TODO: check how update by one data point works here (just like in the original Flip example) - to see if i can
 * replicate the moving normal distribution thing to see if can apply the one-data-point update to GAMMA to make it
 * concept drift and not mixture like the other one turned out.
 */
object try_FlipSketch_IncrementalConceptDrift_AddChangingNormal extends App {

	// TODO start here tomorrow and run the below code in REPL to get dists that are not mean zero to get larger
	//  thdraft starting point
	val dataNo = 1000 //500 //10000
	// changed dataNo (was 1000)
	val draftStart = 100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01

	val SAMPLE_SIZE = 500 //8000 // was 1000

	/**
	 * Concept drift component #1 =
	 * -- Passes a center value and then if past a certain point, reverts back to  earlier values (cycling through
	 * means so that the distributions add up cyclically (?). Otherwise, makes the center as function of velocity
	 * and distance from a given point (draftStart)
	 */
	def center(idx: Int): Double =
		if (draftStart > idx) draftStartingPoint
		else draftStartingPoint + velocity * (idx - draftStart)
	// for console
	// def center(idx: Int): Double = if (draftStart > idx) draftStartingPoint else draftStartingPoint + velocity * (idx - draftStart)

	//def underlying(idx: Int): NumericDist[Double] = NumericDist.normal(center(idx), 10.0, idx)
	def underlying(idxTime: Int)/*(implicit evSmp: Sampling[Double, Distr[Double, NormalDist]])*/: NormalDist =
		NormalDist(center(idxTime),10.0)


	/*val (a1, b1) = (9, 33)
	val (a2, b2) = (47, 49)
	val (a3, b3) = (92, 26)
	val (a4, b4) = (83, 13) // (168, 12) made this crash // good (93, 13)
	val (a5, b5) = (90, 8)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenGamma: GammaDist = GammaDist(a1, b1)
	val redGamma: GammaDist = GammaDist(a2, b2)
	val purpleGamma = GammaDist(a3, b3)
	val orangeGamma = GammaDist(a4, b4)
	val blueGamma = GammaDist(a5, b5)*/

	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located gammas.
	//val gammasIncrementalMove: Seq[GammaDist] = List(greenGamma, redGamma, purpleGamma, orangeGamma, blueGamma)
	/*val smpNorm = implicitly[Sampling[Double, Distr[Double, NormalDist]]]*/
	val normalDists: List[NormalDist] = (0 to dataNo).toList.map(idxTime => underlying(idxTime))
	val normalDistsEveryTenth: List[NormalDist] = normalDists.indices.zip(normalDists)
		.filter{ case (idx, _) => idx	% 10 == 0}.unzip._2.toList

	val normalOneSampleData: List[Double] = normalDists.map(_.sample)
	val normalMultiSampleData: List[List[Double]] = normalDists.map(_.sample(SAMPLE_SIZE).toList).toList

	val (xMIN, xMAX) = (normalMultiSampleData.flatten.min, normalMultiSampleData.flatten.max)

	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(gammaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	val normalOneSampleSketches: Seq[Sketch[Double]] = sketch0 :: sketch0.updateTrace(normalOneSampleData)
	val normalOneEveryTenthSketches = normalOneSampleSketches.indices.zip(normalOneSampleSketches)
		.filter{ case(idx, _) => idx % 10 == 0}.unzip._2

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val normalMultiSampleSketches: Seq[Sketch[Double]] = sketch00 :: sketch00.updateWithMany(normalMultiSampleData)
	val normalMultiEveryTenthSketches = normalMultiSampleSketches.indices.zip(normalMultiSampleSketches)
		.filter{ case(idx, _) => idx % 10 == 0}.unzip._2


	// ----------------------------------------------------------------------------------------------------------------

	// NOTE: Indeed the x-axis represents time because the `updateTrace` from Flip returns one sketch per sampled
	//  value that corresponds to the time (index) it was mapped to, so then you can say that sketch corresponds to that
	//  time  (index).
	// NOTE: must drop 1 sketch because oerwise gives error from manyangled.snowball interpolation that must be xMin
	//  < xMax
	//plotHistSplineFromSketches(gammaOneSampleSketches.drop(2), titleName = Some("Sketches from sample size = 1"))

	println(s"normalOneSampleSketches.length = ${normalOneSampleSketches.length}")
	println(s"normalMultiSampleSketches.length = ${normalMultiSampleSketches.length}")


	plotSketchHistSplines(normalOneEveryTenthSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"One-single Sample: Normal sketches Using Flip Center Drift"),
		//givenColorSeq = Some(List(HTMLNamedColors.blue)),
		graphToColorLabels = Some(normalDistsEveryTenth.map(_.toString))
	)


	plotDensities(normalDistsEveryTenth, HOW_MANY = Some(10))

	plotSketchHistSplines(normalMultiEveryTenthSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE"),
		graphToColorLabels = Some(normalDistsEveryTenth.map(_.toString))
	)
	/*plotSketchHistSplineWithDists(normalMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(normalDists.map(_.toString)),
		originalDists = normalDists.map(_.asInstanceOf[Distr[Real, NormalDist]])
	)*/

}
