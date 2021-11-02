package plotting

import com.cibo.evilplot._
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.numeric.Point

/**
 *
 */
object Example_ScatterPlot extends App {

	val data = Seq.tabulate(100) { i =>
		Point(i.toDouble, scala.util.Random.nextDouble())
	}
	//displayPlot(ScatterPlot(data).render())

	displayPlot(ScatterPlot(data)
		.xAxis()
		.yAxis()
		.frame()
		.xLabel("x")
		.yLabel("y")
		.render() )

	// TODO continue here: https://cibotech.github.io/evilplot/getting-started.html
	// TODO continue in the wiki library-plotting tasks for Evilplot =
}
