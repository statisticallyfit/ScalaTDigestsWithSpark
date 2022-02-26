package workspace_snippets


import org.isarnproject.sketches.TDigest
import utilTest.TestData._

import util.distributionExtensions.distributions._
import util.distributionExtensions.instances._
import util.distributionExtensions.syntax._


import com.cibo.evilplot._
import com.cibo.evilplot.colors.{Color, HTMLNamedColors}
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.numeric._ // Point
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer

import smile.stat.distribution._
/**
 *
 */
object try_Isarn_AddChangingGammas_SMALL extends App {


	val (a1, b1) = (9, 33)
	val (a2, b2) = (47, 49)
	val (a3, b3) = (92, 26)
	val (a4, b4) = (168, 12)

	// Declare the dists https://www.desmos.com/calculator/k5ukou5o1f
	val greenGamma: GammaDist = GammaDist(a1, b1)
	val redGamma: GammaDist = GammaDist(a2, b2)
	val purpleGamma = GammaDist(a3, b3)
	val orangeGamma = GammaDist(a4, b4)

	val gammasMovingRight: Seq[GammaDist] = List(greenGamma, redGamma, purpleGamma, orangeGamma)

	// TODO: changed sample size here so total is smaller, not 40,000
	val shiftData: Seq[Array[Double]] = gammasMovingRight.map(gdist => gdist.sample(500))
	val firstTD: TDigest = TDigest.sketch(shiftData.head, maxDiscrete = MAX_DISCRETE)


	// Creating the sketches and combining them:
	val shiftedSketch: Seq[TDigest] = shiftData.tail
		.map((distSmp: Array[Double]) => TDigest.sketch(distSmp, maxDiscrete = MAX_DISCRETE))
		.scanLeft(firstTD)((ltd: TDigest, rtd: TDigest) => TDigest.combine(ltd, rtd, maxDiscrete = MAX_DISCRETE))
		.drop(1) // TODO look at algebird factory why erikerlandson drops 1 here

	// see how convergence is at the last update (after combining the sketches from ALL the distributions)
	val conceptDriftData = Array.fill[Double](SAMPLE_SIZE){shiftedSketch.last.samplePDF}

	// NOTE FIt the distributions using Mixture model
	val sampleData: Array[Double] = shiftData.flatten.toArray // just combine the individual datas into one large array

	// Create the canonical mixture model (against which to compare to the estimated one at the end)
	// TESTING 1: add same probability to all the gammas
	// TESTING 2: add increasing probability to the ending gammas
	// TODO fix the above implicit conversion function (mydist -> smiledist)
	val a: Mixture.Component = new Mixture.Component(0.25, new GammaDistribution(greenGamma.shape, greenGamma.scale))
	val b: Mixture.Component = new Mixture.Component(0.25,  new GammaDistribution(redGamma.shape, redGamma.scale))
	val c: Mixture.Component = new Mixture.Component(0.25, new GammaDistribution(purpleGamma.shape, purpleGamma.scale))
	val d: Mixture.Component = new Mixture.Component(0.25, new GammaDistribution(orangeGamma.shape, orangeGamma.scale))
	val canonicalMixture: Mixture = new Mixture(a, b, c,d)

	// Estimate the mixture model
	// TODO plot the ESTIMATED mixture model
	val estimatedMixture = ExponentialFamilyMixture.fit(sampleData, a, b, c,d )
	Console.println(estimatedMixture)
}


// Source code = https://stackoverflow.com/a/59894917
/*implicit def convertMyDistToSmileDist[T: Numeric, D](dist: Dist[T, D]): smile.stat.distribution.Distribution = {
	val obj: D = dist.getDist
	//obj.getClass.getConstructors
	//obj.getClass.getDeclaredFields.toList
	// TODO: when using my object (in distributionExtensions package), the `getDeclaredFields` restricts to
	//  returning only the fields of the class and not the superclass, whereas inverse is true for the smile's
	//  distributions --- WHY?
	// NOTE: temporaroy solution = use `getDeclaredFields` when operating on distributionExtensions and use
	//  `getFields` when operating on smile's dists.
	obj.getClass.getDeclaredFields
	/*val args: List[(String, AnyRef)] = obj.getClass.getFields.toList
		.map(f => {
			f.setAccessible(true)
			f.getName -> f.get(obj)
	})*/

	new GammaDistribution(args:_*).
	//TODO methods of doing it:
	1. create manual map from my.dist to smile.dist
	2. above arbitrary gett of arguments (but then problem of how to put them since can't pass arbitrary number
	 when expecting a particular number in the constructor...)

}*/