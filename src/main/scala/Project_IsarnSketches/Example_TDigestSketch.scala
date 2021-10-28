package Project_IsarnSketches

/**
 *
 */

import org.isarnproject.sketches.TDigest


object Example_TDigestSketch extends App {

	val data: Vector[Double] = Vector.fill(10000) { scala.util.Random.nextGaussian() }
	// sketch of some Gaussian data
	val sketch: TDigest = TDigest.sketch(data)
	// the cumulative distribution function of the sketch; cdf(x) at x = 0
	val cdf: Double = sketch.cdf(0.0)
	sketch.samplePDF
	// inverse of the CDF, evaluated at q = 0.5
	val cdfi: Double = sketch.cdfInverse(0.5) //should be close to 0, since mean(gaussian) == 0


	println(s"cdf = $cdf")
	println(s"cdfi = $cdfi")
	// ----------------------------------------------------------

	//val gammaData: Vector[Double] =
}
