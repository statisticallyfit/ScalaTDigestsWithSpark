package plotting

import com.cibo.evilplot._
//import com.cibo.evilplot.colors.HTMLNamedColors
import com.cibo.evilplot.colors._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer


/**
 *
 */
object SeeColors extends App {

	final val NUM_COLORS = 50 //148 // had to count this manually -- another way?

	val heights: Seq[Seq[Int]] = (1 to NUM_COLORS).map(c => Seq.fill[Int](10)(c))
	val barHeights = List.fill[Int](NUM_COLORS)(10)

	val heightsColors: Seq[(Seq[Int], Color)] = heights.zip(Color.stream.take(NUM_COLORS))
	//val t: Seq[Color] = Color.stream.take(10)

	/*val makeBar: (Seq[Int], Color) => Plot = (heights, color) => Histogram(
		heights,
		barRenderer = Some(BarRenderer.default(Some(color))),
		binningFunction = Histogram.density
	)*/


	val makeBarChart: (Seq[Int], Color) => Plot = (heights, color) =>
		BarChart.custom(
			bars = heights.map(h => Bar.apply(h)),
			spacing = Some(20),
			barRenderer = Some(BarRenderer.default(Some(color)))
		)

	val bars: Plot = Overlay(
		heightsColors.map { case (hs, c) => makeBarChart(hs, c) }:_*
	)

	displayPlot( bars
		.xAxis()
		.yAxis()
		.frame()
		.xLabel("x")
		.yLabel("y")
		/*.overlayLegend(x = 0.8)*/
		.render()
	)
}
