package workspace_snippets


import org.isarnproject.sketches.TDigest
import utilTest.TestData._

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._

import util.ConvertMyDistToSmileDist._
import scala.language.implicitConversions

import com.cibo.evilplot._
import com.cibo.evilplot.colors.{Color, HTMLNamedColors}
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.numeric._ // Point
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer

import smile.stat.distribution._

import util.graph.PlotMixture._
/**
 *
 */
object try_IsarnSketch_SmileMixtureFit_AddChangingGammas_SMALL extends App {

	final val SAMPLE_SIZE: Int = 8000

	/**
	 * PARAMETRIC CASE:
	 *
	 * The below is a more advanced example of estimating a mixture model of Gaussian, exponential
	 * and gamma distribution.
	 */
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

	val gammasIncrementalMove: Seq[GammaDist] = List(greenGamma, redGamma, purpleGamma, orangeGamma, blueGamma)

	// TODO: changed sample size here so total is smaller, not 40,000
	val gammaDatas: Seq[Array[Double]] = gammasIncrementalMove.map(gdist => gdist.sample(1000)) //was 500 working
	val firstTD: TDigest = TDigest.sketch(gammaDatas.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	val gammaSketches: Seq[TDigest] = gammaDatas.tail
		.map((distSmp: Array[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(firstTD)((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd, maxDiscrete = MAX_DISCRETE))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here
	// for console
	// val gammaSketches: Seq[TDigest] = gammaDatas.tail.map((distSmp) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE)).scanLeft(firstTD)((ltd, rtd) => TDigest.combine(ltd, rtd, maxDiscrete = MAX_DISCRETE)).drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

	// see how convergence is at the last update (after combining the sketches from ALL the distributions)
	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE){gammaSketches.last.samplePDF}

	// NOTE FIt the distributions using Mixture model
	val sampleData: Array[Double] = gammaDatas.flatten.toArray // just combine the individual datas into one large array

	// Create the canonical mixture model (against which to compare to the estimated one at the end)
	// TESTING 1: add same probability to all the gammas
	// TESTING 2: add increasing probability to the ending gammas
	// TODO fix the above implicit conversion function (mydist -> smiledist)
	/*val a: Mixture.Component = new Mixture.Component(0.2, new GammaDistribution(greenGamma.shape, greenGamma.scale))*/
	val a: Mixture.Component = new Mixture.Component(0.2, greenGamma.toSmileDist[GammaDistribution])
	val b: Mixture.Component = new Mixture.Component(0.2, redGamma.toSmileDist[GammaDistribution])
	val c: Mixture.Component = new Mixture.Component(0.2, purpleGamma.toSmileDist[GammaDistribution])
	val d: Mixture.Component = new Mixture.Component(0.2, orangeGamma.toSmileDist[GammaDistribution])
	val e: Mixture.Component = new Mixture.Component(0.2, blueGamma.toSmileDist[GammaDistribution])
	val canonicalMixture: Mixture = new Mixture(a, b, c, d, e)

	// Estimate the mixture model
	// TODO plot the ESTIMATED mixture model
	val estimatedMixture = ExponentialFamilyMixture.fit(sampleData, a, b, c, d, e)

	Console.println(estimatedMixture)


	/**
	 * NONPARAMETRIC METHOD:
	 *
	 * If the distribution family is not known, nonparametric methods such as kernel density estimation can be used.
	 * Kernel density estimation is a fundamental data smoothing problem where inferences about the population
	 * are made, based on a finite data sample. It is also known as the Parzen window method.
	 */
	val k = new KernelDensity(sampleData)


	/**
	 * PLOTTING
	 */
	plotMixtureTrueEstimated(sampleData, canonicalMixture, estimatedMixture, k)


}
