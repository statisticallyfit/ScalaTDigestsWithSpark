package util

import flip.measure._

/**
 *
 */
object TMeasure  {
	implicit def tMeasureDouble[T](implicit evNum: Numeric[T]): Measure[T] =
		Measure (
			(t: T) => evNum.toDouble(t),
			(d: Double) => BigDecimal.valueOf(d).asInstanceOf[T]
		)

}