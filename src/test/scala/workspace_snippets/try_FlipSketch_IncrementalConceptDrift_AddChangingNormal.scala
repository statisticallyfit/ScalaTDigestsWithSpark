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
object try_FlipSketch_IncrementalConceptDrift_AddChangingNormal extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01

	val SAMPLE_SIZE = 1000 //4000 //500 //8000 // was 1000

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



	// Instead of using the underlying-mean-generating method from Flip, simulating concept drift here by manually
	// providing the differently-located gammas.
	//val gammasIncrementalMove: Seq[GammaDist] = List(greenGamma, redGamma, purpleGamma, orangeGamma, blueGamma)
	/*val smpNorm = implicitly[Sampling[Double, Distr[Double, NormalDist]]]*/
	val normalDists: List[NormalDist] = (0 to dataNo).toList.map(idxTime => underlying(idxTime))
	val normalDistsEveryTenth: List[NormalDist] = normalDists.drop(draftStart).indices.zip(normalDists.drop(draftStart))
		.filter{ case (idx, _) => idx	% 10 == 0}.unzip._2.toList

	val normalOneSampleData: List[Double] = normalDists.map(_.sample)
	val normalMultiSampleData: List[List[Double]] = normalDists.map(_.sample(SAMPLE_SIZE).toList).toList

	//val (xMIN, xMAX) = (normalMultiSampleData.flatten.min, normalMultiSampleData.flatten.max)

	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(gammaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	val normalOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(normalOneSampleData))
		.drop(draftStart) // drop the ones with  mean == 0
	val normalOneEveryTenthSketches = normalOneSampleSketches.indices.zip(normalOneSampleSketches)
		.filter{ case(idx, _) => idx % 10 == 0}
		.unzip._2
		//.drop(draftStart) // drop the ones with mean == 0

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val normalMultiSampleSketches: Seq[Sketch[Double]] = (sketch00 :: sketch00.updateWithMany(normalMultiSampleData))
		.drop(draftStart) // drop the ones with mean == 0
	val normalMultiEveryTenthSketches = normalMultiSampleSketches.indices.zip(normalMultiSampleSketches)
		.filter{ case(idx, _) => idx % 10 == 0}
		.unzip._2



	// ----------------------------------------------------------------------------------------------------------------

	// NOTE: Indeed the x-axis represents time because the `updateTrace` from Flip returns one sketch per sampled
	//  value that corresponds to the time (index) it was mapped to, so then you can say that sketch corresponds to that
	//  time  (index).
	// NOTE: must drop 1 sketch because oerwise gives error from manyangled.snowball interpolation that must be xMin
	//  < xMax
	//plotHistSplineFromSketches(gammaOneSampleSketches.drop(2), titleName = Some("Sketches from sample size = 1"))

	println(s"normalDistsEveryTenth.length = ${normalDistsEveryTenth.length}")
	println(s"normalOneEveryTenthSketches.length = ${normalOneEveryTenthSketches.length}")
	println(s"normalMultiEveryTenthSketches.length = ${normalMultiEveryTenthSketches.length}")



	plotDensities(normalDistsEveryTenth, HOW_MANY = Some(10))

	plotSketchHistSplines(normalOneEveryTenthSketches, //.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"One-single Sample: Normal sketches Using Flip Center Drift (left out first 100 sketches)"),
		//givenColorSeq = Some(List(HTMLNamedColors.blue)),
		graphToColorLabels = Some(normalDistsEveryTenth.drop(draftStart).map(_.toString)),
		originalDists = Some(normalDistsEveryTenth.drop(draftStart)),
		overlayMixture = true
	)

	plotSketchHistSplines(normalMultiEveryTenthSketches, //.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE (left out first 100 " +
			s"sketches)"),
		graphToColorLabels = Some(normalDistsEveryTenth.drop(draftStart).map(_.toString)),
		originalDists = Some(normalDistsEveryTenth.drop(draftStart)),
		overlayMixture = true
	)

}
