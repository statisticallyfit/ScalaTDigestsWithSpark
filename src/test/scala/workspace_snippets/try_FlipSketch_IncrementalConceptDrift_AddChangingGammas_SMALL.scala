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
object try_FlipSketch_IncrementalConceptDrift_AddChangingGammas_SMALL extends App {

	// NOTE: these are the parameters that make the distributions spaced farther apart in the concept drift
	//  illustration (forome reason; otherwise they are too close together .. TODO why?)
	val dataNo = 10000 //1000 //500 //10000
	// changed dataNo (was 1000)
	val draftStart = 300 //100 //300
	val draftStartingPoint = 0.0
	val velocity = 0.01

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

	// Creating list of samples from underlying distribution, length = dataNo = 1000 - taking one sample point from
	// the dist
	val gammaOneSampleData: List[Double] = gammasIncrementalMove.map(_.sample).toList
	val gammaMultiSampleData: List[List[Double]] = gammasIncrementalMove.map(_.sample(SAMPLE_SIZE).toList).toList


	implicit val conf: SketchConf = SketchConf(
		cmapStart = Some(-40.0),
		cmapEnd = Some(40.0) // NOTE changed here from (-20, 20) --- what does this do?
	)


	val sketch0: Sketch[Double] = Sketch.empty[Double]
	val sketch00: Sketch[Double] = Sketch.empty[Double]
	//val firstTD: TDigest = TDigest.sketch(gammaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	val gammaOneSampleSketches: Seq[Sketch[Double]] = (sketch0 :: sketch0.updateTrace(gammaOneSampleData))
		.drop(1) // drop empty sketch at beginning

	// Contains list of incrementally updated sketches - so last one contains all information from previous ones.
	val gammaMultiSampleSketches: Seq[Sketch[Double]] = (sketch00 :: sketch00.updateWithMany(gammaMultiSampleData))
		.drop(1) // drop empty sketch at beginning


	// PLOTTING ----------------------------------------------------------------------------------------------------

	println(s"gammasIncrementalMove.length = ${gammasIncrementalMove.length}")
	println(s"gammaOneSampleSketches.length = ${gammaOneSampleSketches.length}")
	println(s"gammaMultiSampleSketches.length = ${gammaMultiSampleSketches.length}")

	plotSketchHistSplines(normalOneEveryTenthSketches, //.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"One-single Sample: Normal sketches Using Flip Center Drift (left out first 100 sketches)"),
		//givenColorSeq = Some(List(HTMLNamedColors.blue)),
		graphToColorLabels = Some(normalDistsEveryTenth.drop(draftStart).map(_.toString))
	)


	plotDensities(normalDistsEveryTenth, HOW_MANY = Some(10))

	plotSketchHistSplines(normalMultiEveryTenthSketches, //.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Multi-batch samples: Sketches from Sample size = $SAMPLE_SIZE (left out first 100 " +
			s"sketches)"),
		graphToColorLabels = Some(normalDistsEveryTenth.drop(draftStart).map(_.toString))
	)

	// NOTE TEMPORARY COMMENTING
	plotSketchHistSplineWithDists(gammaMultiSampleSketches.drop(1), // drop the empty sketch at beginning
		titleName = Some(s"Sketches from Sample size = $SAMPLE_SIZE"),
		givenColorSeq = Some(List(HTMLNamedColors.green, HTMLNamedColors.red, HTMLNamedColors.purple, HTMLNamedColors
			.orange, HTMLNamedColors.blue)),
		graphToColorLabels = Some(List("green gamma", "red gamma", "purple gamma", "orange gamma", "blue gamma")),
		originalDists = gammasIncrementalMove
	)

}
