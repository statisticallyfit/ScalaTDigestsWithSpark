package workspace_snippets


import com.cibo.evilplot.colors.{Color, HTMLNamedColors}
/*import flip.implicits._
import flip.pdf.Sketch*/

import org.isarnproject.sketches.TDigest

import util.graph.PlotTDigest._
import util.EnhanceFlipSketchUpdate._

import utilTest.TestData.MAX_DISCRETE

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
object try_IsarnSketch_IncrementalConceptDrift_AddChangingGammas_SMALL extends App {
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
	val gammaMultiSampleData: List[Array[Double]] = gammasIncrementalMove.map(gdist => gdist.sample(SAMPLE_SIZE)).toList


	val (xMIN, xMAX) = (gammaMultiSampleData.flatten.min, gammaMultiSampleData.flatten.max)

	// Creating list of sketches which are cumulatively updated from the previous one
	val gammaMultiSampleSketches: Seq[TDigest] = gammaMultiSampleData
		.map((distSmp: Array[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd, maxDiscrete = MAX_DISCRETE))
		.drop(1) // drop the empty one
	//val gammaMultiSampleSketches: Seq[Sketch[Double]] = sketch00 :: sketch00.updateWithMany(gammaMultiSampleData)

	// Creating the sketches and combining them:
	//val gammaOneSampleSketches: Seq[Sketch[Double]] = sketch0 :: sketch0.updateTrace(gammaOneSampleData)



	// ----------------------------------------------------------------------------------------------------------------

	// NOTE: Indeed the x-axis represents time because the `updateTrace` from Flip returns one sketch per sampled
	//  value that corresponds to the time (index) it was mapped to, so then you can say that sketch corresponds to that
	//  time  (index).
	// NOTE: must drop 1 sketch because oerwise gives error from manyangled.snowball interpolation that must be xMin
	//  < xMax
	//plotHistSplineFromSketches(gammaOneSampleSketches.drop(2), titleName = Some("Sketches from sample size = 1"))

	println(s"gammaMultiSampleSketches.length = ${gammaMultiSampleSketches.length}")


	plotSketchHistSplineWithDists(gammaMultiSampleSketches, // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green gamma", "red gamma", "purple gamma", "orange gamma", "blue gamma")),
		originalDists = gammasIncrementalMove
	)
}
