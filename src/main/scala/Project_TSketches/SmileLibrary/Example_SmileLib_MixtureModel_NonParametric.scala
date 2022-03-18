package Project_TSketches.SmileLibrary


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
 * SOURCE = https://hyp.is/t6jNCI8GEeyS1EfGwkEMEQ/haifengl.github.io/statistics.html
 */
object Example_SmileLib_MixtureModel_NonParametric extends App {

	/**
	 * PARAMETRIC CASE:
	 *
	 * The below is a more advanced example of estimating a mixture model of Gaussian, exponential
	 * and gamma distribution.
	 */
	val gaussian = new GaussianDistribution(-2.0, 1.0)
	val exp = new ExponentialDistribution(0.8)
	val gamma = new GammaDistribution(2.0, 3.0)

	// generate the samples
	// TODO: plot the data as histogram
	val sampleData: Array[Double] = Array.fill(500)(gaussian.rand()) ++ Array.fill(500)(exp.rand()) ++ Array.fill(1000)(gamma.rand())


	// define the initial guess of the components in the mixture model
	// TODO: plot the canonical mixture model -- how?
	val a: Mixture.Component = new Mixture.Component(0.3, new GaussianDistribution(0.0, 1.0))
	val b: Mixture.Component = new Mixture.Component(0.3, new ExponentialDistribution(1.0))
	val c: Mixture.Component = new Mixture.Component(0.4, new GammaDistribution(1.0, 2.0))
	val canonicalMixture: Mixture = new Mixture(a, b, c)


	// estimate the mixture model
	// TODO plot the ESTIMATED mixture model
	val estimatedMixture = ExponentialFamilyMixture.fit(sampleData, a, b, c)
	Console.println(estimatedMixture)


	/**
	 * NONPARAMETRIC METHOD:
	 *
	 * If the distribution family is not known, nonparametric methods such as kernel density estimation can be used.
	 * Kernel density estimation is a fundamental data smoothing problem where inferences about the population
	 * are made, based on a finite data sample. It is also known as the Parzen window method.
	 */
	// TODO plot kernel density estimation
	val k = new KernelDensity(sampleData)
	// Now compare getting probability from the kernel densy way (nonparametric) vs. the mixture componenet way
	// (parametric):
	Console.println(s"kernel density prob(3.4): ${k.p(3.4)}") // query probability of getting 3.4
	Console.println(s"parametric way prob(3.4): ${estimatedMixture.p(3.4)}") // query prob of getting 3.4


	// ------------------------------------------------------
	// PLOTTING


	// generate data from canonical mixture:
	val (xMIN, xMAX): (Int, Int) = (sampleData.min.toInt, sampleData.max.toInt)
	println(xMIN, xMAX)

	import breeze.linalg.linspace

	val NUM_POINTS: Int = 2000
	val xs: Seq[Double] = linspace(xMIN, xMAX, length = NUM_POINTS).toScalaVector //(xMIN to xMAX).toList
	val probsCanonicalMixture: Seq[Double] = xs.map(x => canonicalMixture.p(x))
	val probsEstimatedMixture: Seq[Double] = xs.map(x => estimatedMixture.p(x))
	val probsKernel: Seq[Double] = xs.map(x => k.p(x))

	val makeCanonicalMixture: Double => Double = x => canonicalMixture.p(x)
	val makeEstimatedMixture: Double => Double = x => estimatedMixture.p(x)
	val makeKernel: Double => Double = x => k.p(x)


	// Create histogram of data
	val plotHistData: Seq[Double] => Plot = data => Histogram(
		data,
		barRenderer = Some(BarRenderer.default(Some(HTMLNamedColors.blueViolet.copy(opacity = 0.25)))),
		binningFunction = Histogram.density,
		xbounds = Some(Bounds(xMIN, xMAX)) // find the xbounds
	)
	val histPlot: Plot = plotHistData(sampleData)

	// Create the density estimates
	val colors: Seq[Color] = Color.getGradientSeq(3) // for the three probs lists above
	val funcs: Seq[Double => Double] = Seq(
		x => canonicalMixture.p(x),
		x => estimatedMixture.p(x),
		x => k.p(x)
	)
	val funcLabels: Seq[String] = Seq(
		"canonical mixture",
		"estimated mixture",
		"kernel density"
	)
	/*val colorFuncLabelTriple: List[(Color, Double => Double, String)] =
		List(colors, funcs, funcLabels).transpose.map{case List(c, f, l)	=> (c, f,	l)}*/

	val densityPlots: Seq[Plot] = //Overlay(
		colors.zip(funcs).zip(funcLabels).map { case ((color, mixtureFunc), labelFunc) =>
			FunctionPlot.series(
				function = mixtureFunc,
				numPoints = Some(NUM_POINTS), // to be bigger than default of 800
				xbounds = Some(Bounds(xMIN, xMAX)),
				name = labelFunc,
				color = color
				//pathRenderer = Some(PathRenderer.default(color = Some(color), label = Some(labelFunc)))

			)
		} //:_*
	//)


	displayPlot(Overlay((histPlot +: densityPlots): _*)
		.standard()
		.overlayLegend()
		.xbounds(xMIN, xMAX) // args for the function
		.render()
	)

}
