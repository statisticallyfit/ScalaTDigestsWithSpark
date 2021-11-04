package Project_IsarnSketches

/**
 *
 */

//import breeze.stats.distributions.Gamma
//import breeze.stats.distributions.Rand.FixedSeed.randBasis
import org.apache.commons.math3.distribution.GammaDistribution
import org.isarnproject.sketches.TDigest

import util.GraphCDFSpline


object Example_TDigestSketch extends App {

	val data: Vector[Double] = Vector.fill(10000) { scala.util.Random.nextGaussian() }
	// sketch of some Gaussian data
	val sketch: TDigest = TDigest.sketch(data)
	// the cumulative distribution function of the sketch; cdf(x) at x = 0
	val cdf: Double = sketch.cdf(0.0)

	// inverse of the CDF, evaluated at q = 0.5
	val cdfi: Double = sketch.cdfInverse(0.5) //should be close to 0, since mean(gaussian) == 0


	println(s"cdf = $cdf")
	println(s"cdfi = $cdfi")
	// ----------------------------------------------------------

	//long right tail of the pdf, right-skewed
	val gamm: GammaDistribution = new GammaDistribution(2, 2)
	val gammaData: Seq[Double] = gamm.sample(10000)
	// sketch cdf of the gamma data
	val gammaSketch: TDigest = TDigest.sketch(gammaData)
	// the cdf of the sketch; cdf(x) at x = 0
	val cdf0: Double = sketch.cdf(0)
	// inverse of cdf sketch at q = 0.5
	println("\nGamma skewed right:")
	println(s"cdf0 = $cdf0") // 0
	println(s"cdf(1) = ${gammaSketch.cdf(1)}") // 0.5
	println(s"cdf(2) = ${gammaSketch.cdf(2)}") // 0.88
	println(s"cdf(3) = ${gammaSketch.cdf(3)}") // 0.99
	println(s"cdf(4) = ${gammaSketch.cdf(4)}") // 1.00


	println(s"cdf inverse(0.5) = ${gammaSketch.cdfInverse(0.5)}")
	println(s"cdf inverse(0.3) = ${gammaSketch.cdfInverse(0.3)}")


	GraphCDFSpline.showSpliningComparisons(new GammaDistribution(2, 2))



	// -----
	// More localized, centered, normal-looking Gamma (less skewed)

	val gammaDataLocalized: Seq[Double] = new GammaDistribution(15, 3).sample(10000)
	val gammaSketchLocalized: TDigest = TDigest.sketch(gammaDataLocalized)
	println("\nGamma more normal-looking:")
	println(s"cdf(0) = ${gammaSketchLocalized.cdf(0)}")
	println(s"cdf(1) = ${gammaSketchLocalized.cdf(1)}")
	println(s"cdf(2) = ${gammaSketchLocalized.cdf(2)}")
	println(s"cdf(3) = ${gammaSketchLocalized.cdf(3)}")
	println(s"cdf(4) = ${gammaSketchLocalized.cdf(4)}")
	println(s"cdf(7) = ${gammaSketchLocalized.cdf(7)}")
	println(s"cdf(10) = ${gammaSketchLocalized.cdf(10)}")


	println(s"cdf inverse(0.5) = ${gammaSketchLocalized.cdfInverse(0.5)}")
	println(s"cdf inverse(0.3) = ${gammaSketchLocalized.cdfInverse(0.3)}")

	GraphCDFSpline.showSpliningComparisons(new GammaDistribution(15, 3))
}
