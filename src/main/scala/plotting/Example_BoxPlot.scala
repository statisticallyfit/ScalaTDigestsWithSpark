package plotting



import com.cibo.evilplot.plot._
import com.cibo.evilplot._
import com.cibo.evilplot.colors._
import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.{BarRenderer, BoxRenderer}

import scala.util.Random


/**
 *
 */
object Example_BoxPlot extends App {

	val data: Seq[Seq[Double]] = Seq.fill(10)(Seq.fill(Random.nextInt(30))(Random.nextDouble()))

	val plt = BoxPlot(data)

	displayPlot(Overlay(plt)
		.standard(xLabels = (1 to 10).map(_.toString))
		//.xAxis()
		//.yAxis()
		.render()
	)
}
