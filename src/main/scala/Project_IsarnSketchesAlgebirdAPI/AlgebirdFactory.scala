package Project_IsarnSketchesAlgebirdAPI

/**
 * Experiment comparing the old definitions of T-Digest:
 *
 * SOURCE OF CODE = https://github.com/erikerlandson/isarn-sketches-algebird-api/blob/blog/t_digest_sum/src/main/scala/org/isarnproject/sketchesAlgebirdAPI/AlgebirdFactory.scala
 *
 * EXPERIMENT REASON =>
 * (1) https://hyp.is/WSNpXD1MEey45EvUiReeXw/erikerlandson.github
 * .io/blog/2016/12/19/converging-monoid-addition-for-t-digest/
 * (2) http://erikerlandson.github.io/blog/2016/12/19/converging-monoid-addition-for-t-digest/
 */

import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator}
import org.isarnproject.collections.mixmaps.ordered.tree.DataMap
import org.isarnproject.sketches.tdmap.tree.INodeTD
import org.isarnproject.sketches.TDigest
import org.json4s.JObject
import util.GeneralUtil

import scala.collection.immutable
import scala.util.Random
import com.cibo.evilplot._
import com.cibo.evilplot.colors.Clear.opacity
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.renderers.BoxRenderer
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.colors.HTMLNamedColors.{dodgerBlue, fireBrick, red}
import com.cibo.evilplot.geometry.{Drawable, Extent, Rect}

/**
 * Factory functions for generating Algebird objects based on TDigest
 */
object AlgebirdFactory {
	/**
	 * Obtain a new Monoid type-class object based on TDigest
	 * @return A new Monoid with respect to a TDigest with default sketch resolution
	 */
	def tDigestMonoid: Monoid[TDigest] = tDigestMonoid(TDigest.deltaDefault)

	/**
	 * Obtain a new Monoid type-class object based on TDigest
	 * @param delta The TDigest sketch resolution parameter
	 * @return A new Monoid with respect to a TDigest with the given delta
	 */
	def tDigestMonoid(delta: Double): Monoid[TDigest] =
		new Monoid[TDigest] {
			def zero: TDigest = TDigest.empty(delta)
			def plus(x: TDigest, y: TDigest): TDigest = x ++ y
		}

	/**
	 * Obtain an Aggregator for sketching data of a given numeric type, using a TDigest sketch
	 * @tparam N the numeric type to be aggregated
	 * @return An Aggregator that sketches data of type N using TDigest, with default sketch
	 * resolution parameter delta
	 */
	def tDigestAggregator[N](implicit num: Numeric[N]): MonoidAggregator[N, TDigest, TDigest] =
		tDigestAggregator[N](TDigest.deltaDefault)

	/**
	 * Obtain an Aggregator for sketching data of a given numeric type, using a TDigest sketch
	 * @tparam N the numeric type to be aggregated
	 * @param delta the TDigest sketch resolution parameter
	 * @return An Aggregator that sketches data of type N using TDigest, with the given sketch
	 * resolution parameter delta
	 */
	def tDigestAggregator[N](delta: Double)(implicit num: Numeric[N]): MonoidAggregator[N, TDigest, TDigest] =
		Aggregator.appendMonoid((t: TDigest, x: N) => t + x)(tDigestMonoid(delta))
}

import java.io._

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import org.apache.commons.math3.distribution._


object experiment {


	/*type BrokenTDigestAdd = Vector[Double]
	type OrderedTDigestAdd = Vector[Double]*/

	type BrokenTDigestAdd = Seq[Double]
	type OrderedTDigestAdd = Seq[Double]
	type Indices = Seq[Int]





	def algoCombineSort(ltd: TDigest, rtd: TDigest, delta: Double = TDigest.deltaDefault): TDigest = {

		// insert clusters from largest to smallest, instead of randomly
		(ltd.clusters.toVector ++ rtd.clusters.toVector).sortWith((a, b) => a._2 > b._2)
			.foldLeft(TDigest.empty(delta))((d, e) => d + e)
	}

	def algoCombineRandom(ltd: TDigest, rtd: TDigest, delta: Double = TDigest.deltaDefault): TDigest = {
		// randomly shuffle input clusters and re-insert to a new t-digest
		Random.shuffle(ltd.clusters.toVector ++ rtd.clusters.toVector)
			.foldLeft(TDigest.empty(delta))((d, e) => d + e)
	}

	def combine(ltd: TDigest, rtd: TDigest,
			  GOOD_DEF: Boolean = true,
			  delta: Double = TDigest.deltaDefault): TDigest = {

		if (ltd.nclusters <= 1 && rtd.nclusters > 1) {
			combine(rtd, ltd, GOOD_DEF, delta)
		}
		else if (rtd.nclusters == 0) {
			ltd
		}
		else if (rtd.nclusters == 1) {
			// handle the singleton RHS case specially to prevent quadratic catastrophe when
			// it is being used in the Aggregator use case
			val d: DataMap[Double, Double] = rtd.clusters.asInstanceOf[INodeTD].data

			ltd + ((d.key, d.value))

		} else {

			// NOTE: comparing good and bad definitions here.
			GOOD_DEF match {
				case true => algoCombineSort(ltd, rtd, delta)
				case false => algoCombineRandom(ltd, rtd, delta)
			}
		}
	}


	def monoid: Monoid[TDigest] =	new Monoid[TDigest] {

			def zero: TDigest = TDigest.empty(TDigest.deltaDefault)

			def plus(x: TDigest, y: TDigest/*, delta: Double*/): TDigest = combine(x, y)

		// TODO what is the delta here?error if say x.delta as in original code: https://github.com/erikerlandson/isarn-sketches-algebird-api/blob/blog/t_digest_sum/src/main/scala/org/isarnproject/sketchesAlgebirdAPI/AlgebirdFactory.scala#L91
		}



	// Kolmogorov-Smirnov D statistic
	def kolmogorovSmirnovDStatistic(td: TDigest, dist: RealDistribution): Double = {
		val xmin: Double = td.clusters.keyMin.get
		val xmax: Double = td.clusters.keyMax.get
		val step: Double = (xmax - xmin) / 1000.0
		val d: Double = (xmin to xmax by step).iterator
			.map(x => math.abs(td.cdf(x) - dist.cumulativeProbability(x))).max
		d
	}




	final val SAMPLE_SIZE: Int = 10
	final val DATA_SIZE: Int = 10000
	final val N_SUMS: Int = 100
	final val SUM_SAMPLE: Int = 50 // 10


	def collect(mon: Monoid[TDigest], dist: RealDistribution)/*: (BrokenTDigestAdd, OrderedTDigestAdd, Indices)*/ = {

		val raw: Vector[(BrokenTDigestAdd, OrderedTDigestAdd)] = Vector.fill(SAMPLE_SIZE) {
			val data: Vector[Vector[Double]] = Vector.fill(1 + N_SUMS) { Vector.fill(DATA_SIZE) { dist.sample } }

			// NOTE: monoidal addition as defined in the original paper (bad def)
			val oldDef: BrokenTDigestAdd = data
				.map((distSample: Vector[Double]) => TDigest.sketch(distSample)) //vec of tdigests
				.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) =>
					combine(ltd, rtd, GOOD_DEF = false))
				.drop(1)
				.map((tdigest: TDigest) => kolmogorovSmirnovDStatistic(tdigest, dist)) // vector of doubles KSDs

			// NOTE: experimental definition where clusters are inserted from largest to smallest
			val fixedDef: OrderedTDigestAdd = data
				.map((distSample: Vector[Double]) => TDigest.sketch(distSample)) //vec of tdigests
				.scanLeft(TDigest.empty())((ltd: TDigest, rtd: TDigest) =>
					combine(ltd, rtd, GOOD_DEF = true))
				.drop(1)
				.map((tdigest: TDigest) => kolmogorovSmirnovDStatistic(tdigest, dist)) // vector

			(oldDef, fixedDef)
		}

		val step: Int = math.max(1, N_SUMS / SUM_SAMPLE)
		val jvals: Range = 0 to N_SUMS by step // pick out by num monoidal additions
		val oldDef: Seq[Seq[Double]] = jvals.map(j => raw.map(_._1(j))) ///jvals.flatMap(j => raw.map(_._1(j)))
		val fixedDef: Seq[Seq[Double]] = jvals.map(j => raw.map(_._2(j))) //jvals.flatMap(j => raw.map(_._2(j)))
		val indices: Seq[Seq[Int]] = jvals.map(j => Vector.fill(SAMPLE_SIZE)(j)) //axis? indices? //jvals.flatMap(j =>
		// Vector.fill(sampleSize)(j)) //axis? indices?

		(oldDef, fixedDef, indices)
	}

	def writeJSON(data: Seq[(BrokenTDigestAdd, OrderedTDigestAdd, Indices)], fname: String) {
		val json: Seq[JObject] = data.map { case (ref, exp, jv) =>
			("ref" -> ref) ~ ("exp" -> exp) ~ ("jv" -> jv)
		}
		val out: PrintWriter = new PrintWriter(new File(fname))
		out.println(pretty(render(json)))
		out.close()
	}

}

object AlgebirdFactoryRunner extends App {

	import AlgebirdFactory._
	import experiment._


	/*def run(fname: String) {
		/*writeJSON(
			Vector(
				collect(monoid, new NormalDistribution()),
				collect(monoid, new UniformRealDistribution()),
				collect(monoid, new ExponentialDistribution(1.0))
			),
			fname)*/
		writeJSON(Vector(collect(monoid, new ExponentialDistribution(3.5))), fname)
	}


	val path = "/development/projects/statisticallyfit/github/learningmathstat/ScalaTDigestsWithSpark/src/main/scala" +
		"/Project_IsarnSketchesAlgebirdAPI/JSON_result_nonflat"


	run(path)
	 */




	import scala.math.{max, min}

	val (brok, ord, ind): (Seq[Seq[Double]], Seq[Seq[Double]], Seq[Seq[Int]]) = {
		collect(monoid, new ExponentialDistribution(3.5))
	}
	val (xmin, xmax) = (0, 20) // note: number of monoidal additions, get this number out
	val (ymin, ymax) = (0, max(brok.flatten.max, ord.flatten.max))
	/*println("broken:")
	brok.foreach(v => println(v))
	println("\nordered: ")
	ord.foreach(v => println(v))
	println("\nindices: ")
	ind.foreach(v => println(v))*/

	/*def colorBy(fn: Double => Color): BoxRenderer = new BoxRenderer {
		/*def render(plot: Plot, extent: Extent, category: Bar): Drawable =
			Rect(extent) filled fn(category.values.head)*/

		def render(plot: Plot, extent: Extent, summary: BoxRenderer.BoxRendererContext): Drawable = ???
	}*/
	val bx1 = Some(BoxRenderer.default(Some(red.copy(opacity=0.5)), Some(red), None/*, strokeWidth = Some(10)
	*/))
	val bx2 = Some(BoxRenderer.default(Some(dodgerBlue.copy(opacity=0.5)), Some(dodgerBlue), None/*, Some(3)*/))

	displayPlot(
		Overlay(
			BoxPlot(data = brok, boxRenderer = bx1),
			BoxPlot(data =ord, boxRenderer = bx2) //.standard(xLabels = (1 to 10).map(_.toString))
		)
			.standard(/*xLabels = (1 to 10).map(_.toString)*/)
			.xbounds(xmin, xmax)
			.ybounds(ymin, ymax)
			//.frame()
			.render()
	)

	/*

		// TODO simpler to change above collect() to return the chunks of the indices and then group the 'ref' and 'exp'
		//  by the simple sample size number
		val ind: Seq[Seq[Int]] = GeneralUtil.splitGroups(indFlat)*/

	/*Overlay(
		contourPlot(data),
		ScatterPlot(
			Seq(Point(-71.081754,42.3670272)),
			pointRenderer = Some(PointRenderer.default(color = Some(HTMLNamedColors.crimson))))
	).xLabel("Lon")
		.yLabel("Lat")
		.xbounds(-71.2, -71)
		.ybounds(42.20, 42.4)
		.xAxis()
		.yAxis()
		.frame()
		.render()*/
}