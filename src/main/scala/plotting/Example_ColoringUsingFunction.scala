package plotting



import com.cibo.evilplot._
import com.cibo.evilplot.plot._
import com.cibo.evilplot.colors.HTMLNamedColors.{dodgerBlue, red}
import com.cibo.evilplot.colors._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.geometry.{Drawable, Extent, Rect}
import com.cibo.evilplot.numeric.Bounds
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.{BarRenderer, BoxRenderer}


/**
 * SOURCE = https://cibotech.github.io/evilplot/custom-renderers.html#coloring-data-using-a-function
 */

// TODO goal: to plot bars , each a different color --- not working!

object Example_ColoringUsingFunction {
	// TODO see evilplot docs coloring using function

	val NUM_COLORS = 50

	val barHeights: Seq[Double] = Seq.fill[Double](NUM_COLORS)(10.0)
	val colorSeq: Seq[Color] = Color.getGradientSeq(NUM_COLORS)
	val heightColorZipped: Seq[(Int, Double, Color)] =
		(1 to NUM_COLORS)
			.zip(barHeights)
			.zip(colorSeq)
			.map{ case ((index, height), color) => (index, height, color) }

	/*def colorBy(fn: Double => Color): BarRenderer = new BarRenderer {
		def render(plot: Plot, extent: Extent, category: Bar): Drawable = {
			Rect(extent) filled fn(category.values.head)
		}
	}

	val coloring: Int => Color = (index: Int) =>
		heightColorZipped*/

	val seqBars: Seq[Plot] = heightColorZipped.map{ case (i, h, c) =>
		BarChart.custom(
			bars = Seq(Bar(h)), // one element only
			barRenderer = Some(BarRenderer.default(color = Some(c))),
			spacing = Some(20)
		)

	}

	val plt: Drawable = Overlay(seqBars:_*)
		.standard(xLabels = (1 to NUM_COLORS).map(_.toString))
		.xAxis(/*labels = colorSeq.map(_.toString)*/)
		.yAxis()
		//.hline(0)
		.frame()
		.render()

	displayPlot(plt)

}
