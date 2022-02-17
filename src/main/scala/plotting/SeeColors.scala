package plotting

import com.cibo.evilplot._
import com.cibo.evilplot.plot._
import com.cibo.evilplot.colors.HTMLNamedColors.{dodgerBlue, red}
import com.cibo.evilplot.colors._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.{BarRenderer, BoxRenderer}


/**
 *
 */
// TODO goal: to plot bars , each a different color --- not working!

object SeeColors extends App {

	final val NUM_COLORS = 50 //148 // had to count this manually -- another way?

	//val heights: Seq[Seq[Int]] = (1 to NUM_COLORS).map(c => Seq.fill[Int](10)(c))
	val barHeights = (1 to NUM_COLORS).map(c => Seq.fill[Int](10)(c))
		//List.fill[Int](NUM_COLORS)(10)
	println(barHeights)

	val heightsColors: Seq[(Seq[Int], Color)] = barHeights.zip(Color.stream.take(NUM_COLORS))
	println(heightsColors)
	//val t: Seq[Color] = Color.stream.take(10)



	val makeBoxPlot: (Seq[Int], Color) => Plot = (hs, color) =>
		BoxPlot(
			data = Seq(hs.map(_.toDouble)),
			boxRenderer = Some(BoxRenderer.default(fillColor = Some(color), strokeColor = Some(color)))
		)

	displayPlot(Overlay(heightsColors.map{ case (hs, c) => makeBoxPlot(hs, c) }:_* )
		.xAxis()
		.yAxis()
		.frame()
		.xLabel("x")
		.yLabel("y")
		/*.overlayLegend(x = 0.8)*/
		.render()
	)
}
