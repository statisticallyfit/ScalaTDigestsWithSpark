//package plotting
//
//import breeze.linalg._
//import breeze.numerics._
////import breeze.plot._
//
////import plotly._, element._, layout._, Plotly._
//import plotly._
//import plotly.element._
//import plotly.layout._
//
///**
// *
// */
//object Example_HistogramOverlay extends App  {
//
//	implicit def fromDenseVectorDouble(v: DenseVector[Double]): Sequence = v.toArray.toVector
//	implicit def fromDenseVectorString(v: DenseVector[String]): Sequence = v.toArray.toVector
//
//
//
//	val r = DenseVector.rand(500)
//	val x1 = r * 5.0
//	val x2 = r * 10.0
//	val y1 = r
//	val y2 = r * 2.0
//
//	val trace1 = Histogram(
//		r * 5.0,
//		r,
//		name = "control",
//		autobinx = false,
//		histnorm = HistNorm.Count,
//		marker = Marker(
//			color = Color.RGBA(255, 10, 102, 0.7),
//			line = Line(
//				color = Color.RGBA(255, 100, 102, 1),
//				width = 1)),
//		opacity = 0.5,
//		xbins = Bins(0.5, 2.8, 0.06))
//
//	val trace2 = Histogram(
//		r * 10.0,
//		r * 2.0,
//		autobinx = false,
//		marker = Marker(
//			color = Color.RGBA(100, 200, 102, 0.7),
//			line = Line(
//				color = Color.RGBA(100, 200, 102, 1),
//				width = 1)),
//		name = "experimental",
//		opacity = 0.75,
//		xbins = Bins(-3.2, 4, 0.06))
//
//	Seq(trace1, trace2).plot(
//		"histogram.html",
//		Layout(
//			bargap = 0.05,
//			bargroupgap = 0.2,
//			barmode = BarMode.Overlay,
//			title = "Sampled Results",
//			xaxis = layout.Axis(title = "Value"),
//			yaxis = layout.Axis(title = "Count")),
//		false,
//		true,
//		true)
//}
