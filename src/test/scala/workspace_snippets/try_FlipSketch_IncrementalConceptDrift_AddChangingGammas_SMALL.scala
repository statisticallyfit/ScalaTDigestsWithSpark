package workspace_snippets



import com.cibo.evilplot.colors.{Color, HTMLNamedColors}

import flip.implicits._
import flip.pdf.Sketch

import util.graph.PlotSketch._
import util.EnhanceFlipSketchUpdate._

import scala.language.implicitConversions
import util.distributionExtensions.distributions._
import util.distributionExtensions.instances.AllInstances._
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
object try_FlipSketch_IncrementalConceptDrift_AddChangingGammas_SMALL extends App {

	val expName = "incremental-cd-gammas"
	//val dataNo = 10000 // TODO DEBUG next reduce sample_size_from_sketch and increasea this one to 50,000
	// changed dataNo (was 1000)
	//val draftStart = 300
	//val draftStartingPoint = 0.0
	//val velocity = 0.01


	val SAMPLE_SIZE = 8000 // was 1000

	/**
	 * Concept drift component #1 =
	 * -- Passes a center value and then if past a certain point, reverts back to  earlier values (cycling through
	 * means so that the distributions add up cyclically (?). Otherwise, makes the center as function of velocity
	 * and distance from a given point (draftStart)
	 */
	/*def center(idx: Int): Double =
		if (draftStart > idx) draftStartingPoint
		else draftStartingPoint + velocity * (idx - draftStart)*/

	//def underlying(idx: Int): NumericDist[Double] = NumericDist.normal(center(idx), 10.0, idx)
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

	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val gammaOneSampleData: List[Double] = gammasIncrementalMove.map(gdist => gdist.sample).toList
	val gammaMultiSampleData: List[List[Double]] = gammasIncrementalMove.map(gdist => gdist.sample(SAMPLE_SIZE).toList).toList


	val (xMIN, xMAX) = (gammaMultiSampleData.flatten.min, gammaMultiSampleData.flatten.max)

	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(gammaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	val gammaOneSampleSketches: Seq[Sketch[Double]] = sketch0 :: sketch0.updateTrace(gammaOneSampleData)

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val gammaMultiSampleSketches: Seq[Sketch[Double]] = sketch00 :: sketch00.updateWithMany(gammaMultiSampleData)


	// ----------------------------------------------------------------------------------------------------------------

	// NOTE: Indeed the x-axis represents time because the `updateTrace` from Flip returns one sketch per sampled
	//  value that corresponds to the time (index) it was mapped to, so then you can say that sketch corresponds to that
	//  time  (index).
	// NOTE: must drop 1 sketch because oerwise gives error from manyangled.snowball interpolation that must be xMin
	//  < xMax
	//plotHistSplineFromSketches(gammaOneSampleSketches.drop(2), titleName = Some("Sketches from sample size = 1"))

	println(s"gammaOneSampleSketches.length = ${gammaOneSampleSketches.length}")
	println(s"gammaMultiSampleSketches.length = ${gammaMultiSampleSketches.length}")

	// HELP this doesn't work - the function samples 8000 but of course still gets a list of just -40 ... how does
	//  Flip do it - TODO try normal instead of gamma to see if it works as in the given Flip example (using normal)
	/*plotSketchHistSplines(Seq(gammaOneSampleSketches.last), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("blue gamma"))
	)*/


	// NOTE TEMPORARY COMMENTING
	plotSketchHistSplineWithDists(gammaMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green gamma", "red gamma", "purple gamma", "orange gamma", "blue gamma")),
		originalDists = gammasIncrementalMove
	)

}
